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
    /// <summary>
    /// Declaration syntax tree for C/++
    /// functions.
    /// </summary>
    type CppFunctionDecl = {
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
        
    let rec findMdDeclType (decl: char list) : MangledDeclType * char list =
        match decl with
        // Primitives
        | 'v' :: tail -> MdPrimitive "void", tail
        | 'i' :: tail -> MdPrimitive "int", tail
        | 'f' :: tail -> MdPrimitive "float", tail
        | 'd' :: tail -> MdPrimitive "double", tail
        | 'c' :: tail -> MdPrimitive "char", tail
        // Memory characters
        | 'P' :: tail ->
            let baseType, rest = findMdDeclType tail
            MdPointer baseType, rest
        | 'R' :: tail ->
            let baseType, rest = findMdDeclType tail
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
            let baseType, rest = findMdDeclType tail
            // for arguments not for functions
            MdImmutable ("const", baseType), rest
        | 'V' :: tail ->
            let baseType, rest = findMdDeclType tail
            MdVolatile ("volatile", baseType), rest
        | c -> 
            // atom in this context means --
            // this is unknown primitive type, supported by compiler
            MdPrimitive $"atom", List.tail decl
    
    let findMethodQualifiers (chars: char list) : bool * bool * char list =
        let rec loop isConst isVolatile rest =
            match rest with
            | 'K' :: 'V' :: tail -> loop true true tail  // const volatile
            | 'K' :: tail -> loop true isVolatile tail   // const
            | 'V' :: tail -> loop isConst true tail      // volatile
            | _ -> (isConst, isVolatile, rest)
        loop false false chars
        
        
    let swapMethodQualifiers (lst: char list) =
        if List.contains 'N' lst then
            
            let removeFirst x lst =
                let rec loop acc = function
                    | [] -> false, List.rev acc
                    | h :: t when h = x -> true, (List.rev acc) @ t
                    | h :: t -> loop (h :: acc) t
                loop [] lst

            let constFound, lst1 = removeFirst 'K' lst
            let volatileFound, lst2 = removeFirst 'V' lst1
            
            let idx = List.findIndex (fun el -> el = 'N') lst2
            
            let before = List.take idx lst2
            let after = List.skip idx lst2
            
            let toInsert = 
                [ if constFound then yield 'K'
                  if volatileFound then yield 'V' ]
            
            before @ toInsert @ after
        else
            lst
    
    [<CompiledName "DeMangle">]
    let demangle (input: string) : CppFunctionDecl option =
        let chars = input
                    |> List.ofSeq
                    |> swapMethodQualifiers
                    
        match chars with
        | '_' :: 'Z' :: rest ->
            // 1. Qualifiers
            // (const/volatile/virtual)
            let (isConst, isVolatile, restAfterQualifiers) = findMethodQualifiers rest
            
            // 2. Naming
            let nameParts, restAfterName = 
                match restAfterQualifiers with
                | 'N' :: tail ->
                    let names, remaining = getNestedName tail
                    
                    (names, remaining)
                | _ -> 
                    try
                        let name, remaining = getMString restAfterQualifiers
                        ([name], remaining)
                    with _ ->
                        ([], restAfterQualifiers)
            
            // 3. Args / return
            let parameters, returnType, remaining = 
                if restAfterName.IsEmpty then
                    ([], MdPrimitive "void", [])
                else
                    let rec parseParams acc rest depth =
                        if depth > 100 then
                            (List.rev acc, MdPrimitive "arg!", rest)
                        else
                            match rest with
                            | [] -> (List.rev acc, MdPrimitive "void", [])
                            | _ ->
                                try
                                    let param, newRest = findMdDeclType rest
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
                            paramsList |> List.take (paramsList.Length - 1)
                    
                    (parameters, returnType, [afterParams])
                    
            let namespaceParts = 
                if nameParts.Length > 1 then 
                    nameParts[0..nameParts.Length - 2] 
                else []
            
            let className = 
                if nameParts.Length > 1 then 
                    nameParts[nameParts.Length - 2] 
                else String.Empty
            
            let funcName = 
                if nameParts.IsEmpty then "__no_name" 
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
        | _ -> None
    
    let rec typeToCppDecl (mdType: MangledDeclType) : string =
        match mdType with
        | MdPrimitive s -> s
        | MdObject(path, _) ->
            String.concat "::" path
        | MdTemplate(baseType, args) ->
            let argsStr = args
                          |> List.map typeToCppDecl
                          |> String.concat ", "
            $"{typeToCppDecl baseType}<{argsStr}>"
        | MdPointer baseType -> $"{typeToCppDecl baseType}*"
        | MdReference baseType -> $"{typeToCppDecl baseType}&"
        | MdImmutable(_, baseType) -> $"const {typeToCppDecl baseType}"
        | MdVolatile(_, baseType) -> $"volatile {typeToCppDecl baseType}"

    let getCppDecl (pto: CppFunctionDecl option) : string =
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
    let main _args : int =
        let tests = [
            "_ZN7MyClass6methodEiKPiRiv" // void ::MyClass::method [OK]
            "_ZNK7MyClass6methodEiKPiRiv" // const void ::MyClass::method [OK]
            "_ZNV7MyClass6methodEiVPiRiv" // volatile void ::MyClass::method [OK]
            "_ZNKV7MyClass6methodEiViRiv" // const volatile void ::MyClass::method [OK]
            
            //"_ZNSt6vectorIiSaIiEE5beginEv"
        ]
        tests
            |> List.iter (fun f ->
                f
                |> demangle
                |> getCppDecl // namespace=class
                |> printfn "%s")
        0