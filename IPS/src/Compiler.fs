module Compiler

open System
open System.IO
open Lexer
open Parser
open TypeChecker
open CodeGen

type CompilerOptions = {
    InputFile: string
    OutputFile: string option
    Verbose: bool
}

type CompilationStage =
    | Lexing
    | Parsing
    | TypeChecking
    | CodeGeneration

// Using standard Result type to match Program.fs
type CompilationError = {
    Stage: CompilationStage
    Message: string
}

let compile (options: CompilerOptions) : Result<string, CompilationError> =
    try
        // Read source code
        let sourceCode = File.ReadAllText(options.InputFile)
        
        // Lexical analysis
        if options.Verbose then printfn "Starting lexical analysis..."
        let tokens = tokenize sourceCode
        
        // Parsing
        if options.Verbose then printfn "Starting parsing..."
        match parse tokens with
        | Parser.Error error -> 
            Error { Stage = Parsing; Message = sprintf "Parse error at Line %d, Column %d: %s" error.Position.Line error.Position.Column error.Message }
        | Parser.Success ast ->
            
            // Type checking
            if options.Verbose then printfn "Starting type checking..."
            match typeCheck ast with
            | TypeChecker.Error error -> 
                Error { Stage = TypeChecking; Message = sprintf "Type error: %s" error.Message }
            | TypeChecker.Success typedAst ->
                
                // Code generation (generates LLVM IR)
                if options.Verbose then printfn "Starting code generation (LLVM IR)..."
                match generateCode typedAst with
                | CodeGen.Error error -> 
                    Error { Stage = CodeGeneration; Message = sprintf "Code generation error: %s" error.Message }
                | CodeGen.Success llvmIR ->
                    
                    // Write LLVM IR to a temporary file
                    let llvmFile = Path.ChangeExtension(options.InputFile, ".ll")
                    File.WriteAllText(llvmFile, llvmIR)
                    
                    // Determine output file
                    let outputFile = 
                        match options.OutputFile with
                        | Some file -> file
                        | None -> Path.ChangeExtension(options.InputFile, 
                                                      if Environment.OSVersion.Platform = PlatformID.Win32NT 
                                                      then ".exe" else "")
                    
                    // Compile LLVM IR to machine code using LLVM tools (requires llc and clang to be installed)
                    if options.Verbose then printfn "Compiling LLVM IR to machine code..."
                    
                    try
                        // Use clang to compile directly from LLVM IR to executable
                        let clangArgs = sprintf "%s -o %s" llvmFile outputFile
                        let clangProcess = System.Diagnostics.Process.Start("clang", clangArgs)
                        clangProcess.WaitForExit()
                        
                        if clangProcess.ExitCode <> 0 then
                            Error { Stage = CodeGeneration; Message = "Failed to compile LLVM IR to executable" }
                        else
                            if options.Verbose then 
                                printfn "Compilation successful. Executable written to %s" outputFile
                            Ok outputFile
                    with
                    | ex -> Error { Stage = CodeGeneration; Message = sprintf "LLVM compilation error: %s" ex.Message }
    with
    | ex -> Error { Stage = Lexing; Message = sprintf "Compilation error: %s" ex.Message }