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
        | MdVirtual of string * MangledDeclType
    /// <summary>
    /// Itanium ABI substitution type
    /// </summary>
    type Substitution =
        | NameSub of string list   // Для имён: ["ns"; "foo"]
        | TypeSub of MangledDeclType  // Для типов: MdObject(...)
    /// <summary>
    /// Storage of substitutions
    /// </summary>
    type ParseState = {
        Substitutions: Substitution list
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
        Virtual: bool
    }
    let rec resolveSubstitution (code: char) (state: ParseState) : Substitution option * ParseState =
        if Char.IsDigit code then
            let index = int (string code)
            if index < state.Substitutions.Length then
                let subst = state.Substitutions[index]
                (Some subst, state)
            else
                (None, state)
        else
            let newSubst = 
                match code with
                | 'a' -> NameSub ["std"; "allocator"]
                | 'b' -> NameSub ["std"; "basic_string"]
                | 's' -> NameSub ["std"; "string"]
                | 'i' -> NameSub ["std"; "basic_istream"]
                | 'o' -> NameSub ["std"; "basic_ostream"]
                | 'd' -> NameSub ["std"; "basic_iostream"]
                | 't' -> TypeSub (MdObject(["std"], MdPrimitive "std"))
                | _ -> NameSub ["std_" + string code]
            
            let newState = { 
                state with Substitutions = newSubst :: state.Substitutions 
            }
            (Some newSubst, newState)
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
    /// <param name="state">substitution state</param>
    let getNestedName (input: char list, state: ParseState) : string list * char list * ParseState =
        let rec loop acc currentState rest =
            match rest with
            | 'S' :: code :: tail -> 
                match resolveSubstitution code currentState with
                | Some (NameSub names), newState ->
                    // extract names before add
                    loop (List.rev names @ acc) newState tail
                | _ ->
                    loop ($"::{code}" :: acc) currentState tail
            | 'E' :: tail -> 
                (List.rev acc, tail, currentState)
            | _ ->
                match rest with
                | c :: _ when Char.IsDigit(c) ->
                    let name, r = getMString rest
                    let newFullName = (List.rev acc) @ [name]
                    let s = { 
                        currentState with Substitutions = [NameSub [name]; NameSub newFullName] @ currentState.Substitutions 
                    }
                    loop (name :: acc) s r
                | _ -> 
                    (List.rev acc, rest, currentState)
        loop [] state input
    /// <summary>
    /// Matches type of syntax part and translate it
    /// on C/++ syntax
    /// </summary>
    /// <param name="decl">function's bytes</param>
    /// <param name="state">substitution state</param>
    let rec findMdDeclType (decl: char list, state: ParseState) : MangledDeclType * char list * ParseState =
        match decl with
        | 'S' :: code :: tail ->
            match resolveSubstitution code state with
            | Some (TypeSub t), newState -> 
                (t, tail, newState)
            | _ -> 
                (MdPrimitive $"::S{code}", tail, state)
        // Primitives
        | 'v' :: tail -> MdPrimitive "void", tail, state
        | 'w' :: tail -> MdPrimitive "wchar_t", tail, state
        | 'c' :: tail -> MdPrimitive "char", tail, state
        | 'i' :: tail -> MdPrimitive "int", tail, state
        | 'f' :: tail -> MdPrimitive "float", tail, state
        | 'd' :: tail -> MdPrimitive "double", tail, state
        | 'l' :: tail -> MdPrimitive "long", tail, state
        // Memory characters
        | 'P' :: tail ->
            let baseType, rest, s = findMdDeclType (tail, state)
            (MdPointer baseType, rest, s)
        | 'R' :: tail ->
            let baseType, rest, s = findMdDeclType (tail, state)
            (MdReference baseType, rest, s)
        // C++ moment
        | 'N' :: tail ->
            let names, rest, s = getNestedName (tail, state)
            // object in the context of types matching
            // means the c++ coreutils or other entity
            // in example: std::allocator, std::vector
            MdObject (names, MdPrimitive "object"), rest, s
        | 'I' :: tail -> // 'Template<...'
            let rec parseTemplateArgs acc rest state depth =
                if depth > 10 then (List.rev acc, rest, state) // rec limit
                else
                    match rest with
                    | 'E' :: tail -> (List.rev acc, tail, state)
                    | 'I' :: tail -> // template in template
                        let nestedArgs, afterNested, newState = parseTemplateArgs [] tail state (depth + 1)
                        let nestedTemplate = MdTemplate(MdPrimitive "nested", nestedArgs)
                        parseTemplateArgs (nestedTemplate :: acc) afterNested newState depth
                    | _ ->
                        let argType, newRest, newState = findMdDeclType (rest, state)
                        parseTemplateArgs (argType :: acc) newRest newState depth
                        
            let templateArgs, afterTemplate, s = parseTemplateArgs [] tail state 0
            
            let baseType =
                match templateArgs with
                | [] -> MdPrimitive "unknown_template"
                | h :: _ -> h
        
            (MdTemplate(baseType, templateArgs), afterTemplate, s)
        // Immutable entry
        | 'K' :: tail ->
            let baseType, rest, s = findMdDeclType(tail, state)
            // for arguments not for functions
            MdImmutable ("const", baseType), rest, s
        | 'V' :: tail ->
            let baseType, rest, s = findMdDeclType (tail, state)
            MdVolatile ("volatile", baseType), rest, s
        | 'G' :: tail ->
            let baseType, rest, s = findMdDeclType (tail, state)
            MdVirtual ("virtual", baseType), rest, s
        | _ -> 
            // atom in this context means --
            // this is unknown primitive type, supported by compiler
            MdPrimitive "atom", List.tail decl, state
    /// <summary>
    /// Matches and points which function's qualifiers
    /// are available 
    /// </summary>
    /// <param name="chars">function's bytes</param>
    let findMethodQualifiers (chars: char list) : bool * bool * bool * char list =
        let rec loop isConst isVolatile isVirtual rest =
            match rest with
            | 'K' :: 'V' :: 'G' :: tail -> loop true true true tail
            | 'K' :: 'V' :: tail -> loop true true isVolatile tail
            | 'K' :: 'G' :: tail -> loop true true isVirtual tail
            | 'V' :: 'G' :: tail -> loop isConst true true tail
            | 'K' :: tail -> loop true isVolatile isVirtual tail 
            | 'V' :: tail -> loop isConst true isVirtual tail
            | 'G' :: tail -> loop isConst isVolatile true tail
            | _ -> (isConst, isVolatile, isVirtual, rest)
            
        loop false false false chars
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
            let virtualFound, _lst3 = removeFirst 'G' lst1
            
            let idx = List.findIndex (fun el -> el = 'N') lst2
            
            let before = List.take idx lst2
            let after = List.skip idx lst2
            
            
            let toInsert = 
                [ if constFound then yield 'K'
                  if volatileFound then yield 'V'
                  if virtualFound then yield 'G']
            
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
                let isConst, isVolatile, isVirtual, restAfterQualifiers = findMethodQualifiers rest
                let initState = { Substitutions = [] }
                
                let nameParts, restAfterName, nameState = 
                    match restAfterQualifiers with
                    | 'N' :: tail ->
                        getNestedName (tail, initState)
                    | _ -> 
                        try
                            let name, remaining = getMString restAfterQualifiers
                            ([name], remaining, initState)
                        with _ ->
                            ([], restAfterQualifiers, initState)
                
                let rec parseParams acc rest depth state =
                    if depth > 100 then (List.rev acc, MdPrimitive "arg!", rest, state)
                    else
                        match rest with
                        | [] -> (List.rev acc, MdPrimitive "void", [], state)
                        | _ ->
                            try
                                let param, newRest, newState = findMdDeclType (rest, state)
                                parseParams (param :: acc) newRest (depth + 1) newState
                            with ex ->
                                (List.rev acc, MdPrimitive "arg", rest, state)
                
                let paramsList, returnType, remaining, _ = 
                    parseParams [] restAfterName 0 nameState
                        
                let returnType = 
                    if paramsList.IsEmpty then 
                        MdPrimitive "void" 
                    else 
                        paramsList
                            |> List.last
                
                let parameters = 
                    if paramsList.IsEmpty then [] 
                    else
                        paramsList
                            |> List.take (paramsList.Length - 1)
                    
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
                    Virtual = isVirtual 
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
            String.concat "" path //
        | MdTemplate(baseType, args) ->
            let argsStr = args
                          |> List.map typeToCppDecl
                          |> String.concat ", "
            $"{typeToCppDecl baseType}<{argsStr}>"
        | MdPointer baseType -> $"{typeToCppDecl baseType}*"
        | MdReference baseType -> $"{typeToCppDecl baseType}&"
        | MdImmutable(_, baseType) -> $"const {typeToCppDecl baseType}"
        | MdVolatile(_, baseType) -> $"volatile {typeToCppDecl baseType}"
        | MdVirtual(_, baseType) -> $"virtual {typeToCppDecl baseType}"
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
                if pt.Volatile then "volatile"
                if pt.Virtual then "virtual" ]
            |> String.concat " "
            
        // building C++ declaration rule
        // name_space::CEntity::mFunctional
        let fullName = 
            [ if not (String.IsNullOrEmpty pt.Namespace) then pt.Namespace
              // if not (String.IsNullOrEmpty pt.Class) then pt.Class
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
            "_Z5stateRic"
            "_ZNSt3_In4wardE"
            "_ZN2ns3fooIS0_IiEEEv"
        ]
        tests
            |> List.iter (fun f ->
                f
                |> demangle
                |> getCppDecl // namespace=class
                |> printfn "%A")
        0