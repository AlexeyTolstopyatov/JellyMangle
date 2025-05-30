namespace JellyMangle.Itanium
module ItaniumAbiProcessor =
    open System
    /// <summary>
    /// Describes all available syntax parts
    /// of C/++ functions declaration.
    /// </summary>
    type MangledDeclType =
        | MdPrimitive of string
        | MdObject of string list * MangledDeclType
        | MdTemplate of MangledDeclType * MangledDeclType list
        | MdPointer of MangledDeclType
        | MdReference of MangledDeclType
        | MdImmutable of string * MangledDeclType
        | MdVolatile of string * MangledDeclType
        
    type MangledSubstitutionType = {
        
    }
    /// <summary>
    /// Declaration syntax parts for C/++
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
    /// <summary>
    /// Matches and returns expected ASCII string
    /// with other function's bytes
    /// </summary>
    /// <param name="chars">function's bytes</param>
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
    /// <summary>
    /// Returns namespace word of required function
    /// </summary>
    /// <param name="input">function's bytes</param>
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
    /// <summary>
    /// Matches type of syntax part and translate it
    /// on C/++ syntax
    /// </summary>
    /// <param name="decl">function's bytes</param>
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
        // C++ moment
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
        | _ -> 
            // atom in this context means --
            // this is unknown primitive type, supported by compiler
            MdPrimitive "atom", List.tail decl
    /// <summary>
    /// Matches and points which function's qualifiers
    /// are available 
    /// </summary>
    /// <param name="chars">function's bytes</param>
    let findMethodQualifiers (chars: char list) : bool * bool * char list =
        let rec loop isConst isVolatile rest =
            match rest with
            | 'K' :: 'V' :: tail -> loop true true tail  // const volatile
            | 'K' :: tail -> loop true isVolatile tail   // const
            | 'V' :: tail -> loop isConst true tail      // volatile
            | _ -> (isConst, isVolatile, rest)
        loop false false chars
    /// <summary>
    /// Matches and Replaces function's qualifiers
    /// for next recognition
    /// </summary>
    /// <param name="lst">function's bytes</param>
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
    /// <summary>
    /// Translates mangled C/++ function string
    /// to <see cref="CppFunctionDecl"/> struct.
    /// If you need translate <see cref="CppFunctionDecl"/> to
    /// ready C/++ function's prototype - use <see cref="getCppDecl"/>
    /// </summary>
    /// <param name="input">Expected Mangled Itanium ABI prototype string</param>
    [<CompiledName "DeMangle">]
    let demangle (input: string) : CppFunctionDecl option =
        let chars = input
                    |> List.ofSeq
                    |> swapMethodQualifiers
                    
        match chars with
        | '_' :: 'Z' :: rest ->
            // 1. Qualifiers
            // (const/volatile/virtual)
            let isConst, isVolatile, restAfterQualifiers = findMethodQualifiers rest
            
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
            let parameters, returnType, _remaining = 
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
    /// <summary>
    /// Translates parts of syntax to strings
    /// of C/++ parts of function's prototype
    /// </summary>
    /// <param name="mdType">Part of <see cref="CppFunctionDecl"/></param>
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
    /// <summary>
    /// Translates structure of recognized Itanium ABI
    /// function to declaration string
    /// </summary>
    /// <param name="pto"><see cref="Option[CppFunctionDecl]"/></param>
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
            "_ZN7MyClass6methodEiKPiRiv"
            "_ZNK7MyClass6methodEiKPiRiv"
            "_ZNV7MyClass6methodEiVPiRiv"
            "_ZNKV7MyClass6methodEiViRiv"
            // C++ templates usage
            // "_ZNSt6vectorIiSaIiEE5beginEv" // substitutions needed. template matching needed
        ]
        tests
            |> List.iter (fun f ->
                f
                |> demangle
                |> getCppDecl // namespace=class
                |> printfn "%s")
        0