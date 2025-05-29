namespace JellyMangle.Itanium
module ItaniumAbiProcessor =
    open System
    
    type MangledDeclType =
        | MdPrimitive of string
        | MdObject of string list * MangledDeclType
        | MdTemplate of MangledDeclType * MangledDeclType list
        | MdPointer of MangledDeclType
        | MdReference of MangledDeclType
        | MdImmutable of string * MangledDeclType
        | MdVolatile of string * MangledDeclType
    
    type ProcedureTemplate = {
        Namespace: string
        Class: string
        Name: string
        Parameters: MangledDeclType list
        Returns: MangledDeclType
        ImMutable: bool
        Volatile: bool
    }
    
    let getMString (chars: char list) : string * char list =
        let rec readDigits acc rest =
            match rest with
            | c :: tail when Char.IsDigit c -> readDigits (acc + string c) tail
            | _ -> (acc, rest)
        
        match chars with
        | [] -> failwith "Character sequence is empty"
        | c :: _ when Char.IsDigit c ->
            let lengthStr, restAfterDigits = readDigits "" chars
            if String.IsNullOrEmpty lengthStr then
                failwith "Length null or empty"
            
            let length = int lengthStr
            if length > restAfterDigits.Length then
                failwithf $"Length %d{length} exceeds remaining chars %d{restAfterDigits.Length}"
            
            let name = restAfterDigits |> List.take length |> Array.ofList |> String
            (name, List.skip length restAfterDigits)
        | _ -> failwithf $"Expected digit, got: %A{chars}"

    let getNestedName (input: char list) : string list * char list =
        let rec loop acc rest =
            match rest with
            | [] -> (List.rev acc, [])
            | 'E' :: tail -> (List.rev acc, tail)
            | _ ->
                try
                    let name, newRest = getMString rest
                    loop (name :: acc) newRest
                with _ ->
                    (List.rev acc, rest)
        loop [] input
        
    let rec getMdDeclType (decl: char list) : MangledDeclType * char list =
        match decl with
        // Primitives
        | 'v' :: tail -> MdPrimitive "void", tail
        | 'i' :: tail -> MdPrimitive "int", tail
        | 'f' :: tail -> MdPrimitive "float", tail
        | 'd' :: tail -> MdPrimitive "double", tail
        | 'c' :: tail -> MdPrimitive "char", tail
        // Memory characters
        | 'P' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdPointer baseType, rest
        | 'R' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdReference baseType, rest
        // C++ la momento
        | 'N' :: tail ->
            let names, rest = getNestedName tail
            // object in the context of types matching
            // means the c++ coreutils or other entity
            // in example: std::allocator, std::vector
            MdObject(names, MdPrimitive "object"), rest
        // Immutable entry
        | 'K' :: tail ->
            let baseType, rest = getMdDeclType tail
            // for arguments not for functions
            MdImmutable ("const", baseType), rest
        | 'V' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdVolatile ("volatile", baseType), rest
        | c -> 
            // atom in this context means --
            // this is unknown primitive type, supported by compiler
            MdPrimitive $"atom::{c}", List.tail decl
    
    let parseMethodQualifiers (chars: char list) : bool * bool * char list =
        let rec loop isConst isVolatile rest =
            match rest with
            | 'K' :: tail -> loop true isVolatile tail
            | 'V' :: tail -> loop isConst true tail
            | _ -> (isConst, isVolatile, rest)
        loop false false chars
    
    [<CompiledName "DeMangle">]
    let demangle (input: string) : ProcedureTemplate option =
        let chars = List.ofSeq input
        match chars with
        | '_' :: 'Z' :: rest ->
            let nameParts, restAfterName = 
                match rest with
                | 'N' :: tail -> 
                    let names, remaining = getNestedName tail
                    (names, remaining)
                | _ -> 
                    try
                        let name, remaining = getMString rest
                        ([name], remaining)
                    with _ ->
                        ([], rest)
                        
            let parseMethodQualifiers (c: char list) : bool * bool * char list =
                let rec loop isConst isVolatile rest =
                    match rest with
                    | 'K' :: tail -> loop true isVolatile tail
                    | 'V' :: tail -> loop isConst true tail
                    | _ -> (isConst, isVolatile, rest)
                loop false false c
            
            // 
            // Namespamce and Qualifiers ordering replaced.
            // broken.
            let isConst, isVolatile, rem = parseMethodQualifiers restAfterName
            
            let parameters, returnType, rem = 
                if rem.IsEmpty then
                    ([], MdPrimitive "void", [])
                else
                    let rec parseParams acc rest depth =
                        if depth > 100 then
                            (List.rev acc, MdPrimitive "arg!", rest) // ! means last primitive from rec
                        else
                            match rest with
                            | [] -> (List.rev acc, MdPrimitive "void", [])
                            | _ ->
                                try
                                    let param, newRest = getMdDeclType rest
                                    parseParams (param :: acc) newRest (depth + 1)
                                with ex ->
                                    (List.rev acc, MdPrimitive "arg", rest)
                    
                    let paramsList, afterParams, _depth =
                        parseParams [] restAfterName 0
                    
                    let returnType = 
                        if paramsList.IsEmpty then 
                            MdPrimitive "void" 
                        else 
                            paramsList |> List.last
                    
                    let parameters = 
                        if paramsList.IsEmpty then [] 
                        else
                            paramsList
                                |> List.take (paramsList.Length - 1)
                    
                    (parameters, returnType, [afterParams])
            
            let namespaceParts = 
                if nameParts.Length > 1 then 
                    nameParts[0..nameParts.Length - 2] 
                else []
            
            let className = 
                if nameParts.Length > 1 then 
                    nameParts[nameParts.Length - 2] 
                else
                    String.Empty
            
            let funcName = 
                if nameParts.IsEmpty then "no_name" 
                else nameParts[nameParts.Length - 1]
            
            Some { 
                Namespace = String.Join("::", namespaceParts)
                Class = className
                Name = funcName
                Parameters = parameters
                Returns = returnType
                ImMutable = isConst
                Volatile = isVolatile
            }
        | _ ->
            None
    
    let rec typeToCppDecl (mdType: MangledDeclType) : string =
        match mdType with
        | MdPrimitive s -> s
        | MdObject(path, _) ->
            String.concat "::" path
        | MdTemplate(baseType, args) ->
            let argsStr = args |> List.map typeToCppDecl |> String.concat ", "
            $"{typeToCppDecl baseType}<{argsStr}>"
        | MdPointer baseType -> $"{typeToCppDecl baseType}*"
        | MdReference baseType -> $"{typeToCppDecl baseType}&"
        | MdImmutable(_, baseType) -> $"const {typeToCppDecl baseType}"
        | MdVolatile(_, baseType) -> $"volatile {typeToCppDecl baseType}"

    let getCppDecl (pto: ProcedureTemplate option) : string =
        match pto with
        | None -> String.Empty
        | Some pt ->
        
        let qualifiers =
              [ if pt.ImMutable then "const"
                if pt.Volatile then "volatile" ]
            |> String.concat " "
            
        // building C++ declaration rule
        // name_space::CEntity::mFunctional
        let fullName = 
            [ if not (String.IsNullOrEmpty pt.Namespace) then pt.Namespace
              if not (String.IsNullOrEmpty pt.Class) then pt.Class
              pt.Name ]
            |> List.filter (fun s -> not (String.IsNullOrEmpty s))
            |> String.concat "::"
        
        // Seeking for parameters
        // mFunctional(...
        let parameters = 
            pt.Parameters
            |> List.map typeToCppDecl
            |> String.concat ", "
        
        // getting return type
        let returnType = typeToCppDecl pt.Returns
        
        // return cppName
        $"{qualifiers} {returnType} {fullName}({parameters})"
    
    [<EntryPoint>]
    let main _args =
        let tests = [
            "_ZN7CString3newEPc"
            "_ZNK7MyClass6methodEv" // void ::MyClass::method
            // // total problems
            //"_ZNSt6vectorIiSaIiEE5beginEv"
            "_Z1fv"
        ]
        
        tests
            |> List.iter (fun name ->
                name
                |> demangle
                |> printfn "%A"
            )
        tests
            |> List.iter (fun f ->
                f
                |> demangle
                |> getCppDecl // namespace=class
                |> printfn "%s")
        0