module Program

open System
open Compiler

let printUsage() =
    printfn "Usage: compiler [options] input-file"
    printfn "Options:"
    printfn "  -o <file>    Specify output file"
    printfn "  -v           Enable verbose output"
    printfn "  -h, --help   Display this help message"

let parseArgs (args: string[]) =
    let rec parseArgsRec (args: string list) (options: CompilerOptions) =
        match args with
        | [] -> Ok options
        | "-o" :: outputFile :: rest ->
            parseArgsRec rest { options with OutputFile = Some outputFile }
        | "-v" :: rest ->
            parseArgsRec rest { options with Verbose = true }
        | "-h" :: _ | "--help" :: _ ->
            Error "Help requested"
        | inputFile :: rest when not (inputFile.StartsWith("-")) && options.InputFile = "" ->
            parseArgsRec rest { options with InputFile = inputFile }
        | unknown :: _ ->
            Error (sprintf "Unknown option or multiple input files: %s" unknown)
    
    parseArgsRec (Array.toList args) { InputFile = ""; OutputFile = None; Verbose = false }

[<EntryPoint>]
let main args =
    match parseArgs args with
    | Error "Help requested" ->
        printUsage()
        0
    | Error msg ->
        printfn "Error: %s" msg
        printUsage()
        1
    | Ok options when options.InputFile = "" ->
        printfn "Error: No input file specified"
        printUsage()
        1
    | Ok options ->
        match compile options with
        | Ok outputFile ->
            printfn "Compilation successful. Output written to %s" outputFile
            0
        | Error error ->
            printfn "Compilation failed during %A stage: %s" error.Stage error.Message
            1