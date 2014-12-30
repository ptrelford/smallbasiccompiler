open System
open System.IO

[<EntryPoint>]
let main argv = 
    if argv.Length = 0 then 
        printfn "Extended Small Basic Compiler"
        printfn ""
        printfn "Please specify a source file to compile"
        exit 0
    let path = argv.[0]
    if not (File.Exists(path)) then
        printfn "File does not exist"
        exit 0
    try
        let source = File.ReadAllText(path)
        let program = Parser.parse source
        let exe = Path.GetFileNameWithoutExtension(path)
        Compiler.compileTo exe program
    with e ->
        printfn "%s" e.Message
    0