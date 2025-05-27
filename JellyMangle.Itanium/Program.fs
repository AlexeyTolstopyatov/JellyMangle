namespace JellyMangle.Itanium

module ItaniumAbiProcessor =
    open System
    
    type MangledDeclType =
        | MdPrimitive of String
        | MdObject of List<String> * MangledDeclType // MdType
        | MdTemplate of MangledDeclType * List<MangledDeclType>
        | MdPointer of MangledDeclType
        | MdReference of MangledDeclType
        | MdImmutable of String * MangledDeclType
    
    type ProcedureTemplate = {
        Namespace: String
        Class: String
        Name: String
        Parameters: List<MangledDeclType>
        Returns: MangledDeclType
        ImMutable: Boolean
    }
    /// <summary>
    /// Get Mangled String from charset
    /// </summary>
    /// <param name="chars"></param>
    let getMString (chars: char list) : String * List<Char> =
        let rec readDigits acc rest =
            match rest with
            | c :: tail when Char.IsDigit c  -> readDigits (acc + string c) tail
            | _ -> (acc, rest)
        
        let lengthStr, restAfterDigits = readDigits "" chars
        if String.IsNullOrEmpty lengthStr then
            failwith "Length null or empty"
        
        let length = int lengthStr
        let name = List.take length restAfterDigits |> Array.ofList |> String
        
        (name, List.skip length restAfterDigits)
    /// <summary>
    /// 
    /// </summary>
    /// <param name="input"></param>
    let getNestedName (input: char list) : List<String> * List<Char> =
        let rec loop acc rest =
            match rest with
            | 'E' :: tail -> (List.rev acc, tail)
            | _ ->
                let name, newRest =
                    getMString rest
                
                loop (name :: acc) newRest
        loop [] input
        
    /// <summary>
    /// Translates parts of declaration
    /// to language objects
    /// </summary>
    /// <param name="decl"></param>
    let rec getMdDeclType (decl: List<Char>) =
        match decl with
        | 'v' :: tail -> MdPrimitive "void", tail
        | 'i' :: tail -> MdPrimitive "int", tail
        | 'f' :: tail -> MdPrimitive "float", tail
        | 'd' :: tail -> MdPrimitive "double", tail
        | 'c' :: tail -> MdPrimitive "char", tail
        | 'P' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdPointer baseType, rest
        | 'R' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdReference baseType, rest
        | 'K' :: tail ->
            let baseType, rest = getMdDeclType tail
            MdImmutable("const", baseType), rest
        | 'N' :: tail ->
            // Shorten variant!!!!
            let names, rest = getNestedName tail
            MdObject(names, MdPrimitive "unknown"), rest
        | _ -> MdPrimitive "unknown", decl
        
    /// <summary>
    /// Reads and matches C++ Template arguments
    /// </summary>
    /// <param name="rest"></param>
    let rec getTemplateArgs (rest: char list) : MangledDeclType list * char list =
        match rest with
        | 'I' :: tail -> 
            let args, remaining = parseTemplateArgs tail
            (args, remaining)
        | _ -> failwith "Not a template"

    and parseTemplateArgs (input: char list) : MangledDeclType list * char list =
        let rec loop acc rest =
            match rest with
            | 'E' :: tail -> (List.rev acc, tail)
            | _ ->
                let argType, newRest =
                    getMdDeclType rest
                
                loop (argType :: acc) newRest
        loop [] input
        
    [<CompiledName "DeMangle">]
    let demangle (input: string) : Option<ProcedureTemplate> =
        let chars = List.ofSeq input
        match chars with
        | '_' :: 'Z' :: rest ->
            // pre-declared keywords matching
            let isConst, restAfterConst = 
                match rest with
                | 'K' :: tail -> (true, tail)
                | _ -> (false, rest)
            
            // nested names matching
            let nameParts, restAfterName = 
                match restAfterConst with
                | 'N' :: tail -> 
                    let names, remaining = getNestedName tail
                    (names, remaining)
                | _ -> 
                    let name, remaining = getMString restAfterConst
                    ([name], remaining)
            
            // return types
            let parameters, returnType, remaining = 
                let rec parseParams acc rest =
                    match rest with
                    | 'E' :: tail -> (List.rev acc, tail)
                    | _ ->
                        let param, newRest = getMdDeclType rest
                        parseParams (param :: acc) newRest
                let paramsList, afterParams = parseParams [] restAfterName
                let returnType, remaining = getMdDeclType afterParams
                (paramsList, returnType, remaining)
            
            // namespace::class::method
            let namespaceParts = 
                if nameParts.Length > 1 then 
                    nameParts.[0 .. nameParts.Length - 2] 
                else []
            let className = 
                if nameParts.Length > 1 then 
                    nameParts.[nameParts.Length - 2] 
                else ""
            let funcName = 
                nameParts.[nameParts.Length - 1]
            
            Some { 
                Namespace = String.Join("::", namespaceParts)
                Class = className
                Name = funcName
                Parameters = parameters
                Returns = returnType
                ImMutable = isConst
            }
        | _ -> None
    
    let testNames () = [
        "_Z3fooiv",
        "foo(void) -> int",
        
        "_ZNK3foo3barEic",
        "const foo::bar(int, char)",
        
        "_ZNSt6vectorIiSaIiEE5beginEv",
        "std::vector<int, std::allocator<int>>::begin(void)"
    ]
    
    [<EntryPoint>]
    let main args =
        demangle "_Z3fooiv"
            |> printfn "%A"
        0