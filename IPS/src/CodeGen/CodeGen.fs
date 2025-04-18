module CodeGen

open System
open System.Text
open System.Collections.Generic
open AST

type CompilationError = {
    Message: string
    Statement: Statement option
}

type CompilationResult<'a> =
    | Success of 'a
    | Error of CompilationError

// LLVM IR Type representation
type LLVMType =
    | I32        // 32-bit integer
    | I8         // 8-bit integer (char)
    | I1         // boolean
    | Void       // void
    | Pointer of LLVMType  // pointer to a type
    | Function of LLVMType list * LLVMType  // function type

// LLVM Value representation
type LLVMValue = {
    Type: LLVMType
    Name: string
}

// Compilation context
type CompilationContext = {
    Module: StringBuilder
    CurrentFunction: StringBuilder option
    Variables: Map<string, LLVMValue>
    NextRegister: int
    NextLabel: int
    StringConstants: Dictionary<string, string>
}

// Create a new compilation context
let createContext() =
    { 
        Module = new StringBuilder()
        CurrentFunction = None
        Variables = Map.empty
        NextRegister = 1
        NextLabel = 0
        StringConstants = new Dictionary<string, string>()
    }

// Get the next register name
let getNextRegister (ctx: CompilationContext) =
    let regName = sprintf "%%r%d" ctx.NextRegister
    let newCtx = { ctx with NextRegister = ctx.NextRegister + 1 }
    (newCtx, regName)

// Get the next label name
let getNextLabel (ctx: CompilationContext) prefix =
    let labelName = sprintf "%s%d" prefix ctx.NextLabel
    let newCtx = { ctx with NextLabel = ctx.NextLabel + 1 }
    (newCtx, labelName)

// Type conversion from AST Type to LLVM Type
let rec toLLVMType typ =
    match typ with
    | TypeChecker.IntType -> I32
    | TypeChecker.StringType -> Pointer I8
    | TypeChecker.BoolType -> I1
    | TypeChecker.VoidType -> Void
    | TypeChecker.FunctionType(paramTypes, returnType) ->
        Function(List.map toLLVMType paramTypes, toLLVMType returnType)
    | _ -> I32  // Default to I32 for unknown types

// LLVM IR type string representation
let rec llvmTypeToString = function
    | I32 -> "i32"
    | I8 -> "i8"
    | I1 -> "i1"
    | Void -> "void"
    | Pointer t -> sprintf "%s*" (llvmTypeToString t)
    | Function(paramTypes, returnType) ->
        let paramStr = String.Join(", ", paramTypes |> List.map llvmTypeToString)
        sprintf "%s (%s)" (llvmTypeToString returnType) paramStr

// Add a global string constant
let addStringConstant (ctx: CompilationContext) (value: string) =
    if ctx.StringConstants.ContainsKey(value) then
        (ctx, ctx.StringConstants.[value])
    else
        let constName = sprintf "@.str.%d" ctx.StringConstants.Count
        let escapedValue = value.Replace("\"", "\\22").Replace("\n", "\\0A")
        
        // Add string constant to module
        ctx.Module.AppendLine(sprintf "%s = private unnamed_addr constant [%d x i8] c\"%s\\00\"" 
                                     constName (value.Length + 1) escapedValue) |> ignore
        
        ctx.StringConstants.Add(value, constName)
        (ctx, constName)

// Add a function declaration to the module
let addFunctionDeclaration (ctx: CompilationContext) (name: string) (paramTypes: LLVMType list) (returnType: LLVMType) =
    let paramTypesStr = String.Join(", ", paramTypes |> List.map llvmTypeToString)
    let signature = sprintf "define %s @%s(%s)" (llvmTypeToString returnType) name paramTypesStr
    
    // Start a new function
    let funcBuilder = new StringBuilder()
    funcBuilder.AppendLine(signature + " {") |> ignore
    
    { ctx with CurrentFunction = Some funcBuilder }

// Finalize the current function and add it to the module
let finalizeFunction (ctx: CompilationContext) =
    match ctx.CurrentFunction with
    | None -> ctx
    | Some funcBuilder ->
        funcBuilder.AppendLine("}") |> ignore
        ctx.Module.Append(funcBuilder.ToString()) |> ignore
        { ctx with CurrentFunction = None }

// Emit LLVM IR instruction to the current function
let emit (ctx: CompilationContext) (instruction: string) =
    match ctx.CurrentFunction with
    | None -> failwith "No active function to emit instructions into"
    | Some funcBuilder ->
        funcBuilder.AppendLine("    " + instruction) |> ignore
        ctx

// Emit a comment in the LLVM IR
let emitComment (ctx: CompilationContext) (comment: string) =
    match ctx.CurrentFunction with
    | None -> ctx
    | Some funcBuilder ->
        funcBuilder.AppendLine(sprintf "    ; %s" comment) |> ignore
        ctx

// Generate LLVM IR for a literal
let generateLiteral (ctx: CompilationContext) (lit: Literal) =
    match lit with
    | IntLiteral value -> 
        (ctx, I32, sprintf "%d" value)
    | StringLiteral value ->
        // Add string constant to module and get a pointer to it
        let (newCtx, constName) = addStringConstant ctx value
        let (newCtx2, reg) = getNextRegister newCtx
        
        // Get pointer to the first character (getelementptr)
        let instr = sprintf "%s = getelementptr [%d x i8], [%d x i8]* %s, i32 0, i32 0" 
                           reg (value.Length + 1) (value.Length + 1) constName
        
        (emit newCtx2 instr, Pointer I8, reg)
    | BoolLiteral value ->
        (ctx, I1, if value then "1" else "0")

// Generate LLVM IR for a binary operation
let generateBinaryOp (ctx: CompilationContext) (left: LLVMValue) (op: BinaryOp) (right: LLVMValue) =
    let (ctx1, resultReg) = getNextRegister ctx
    
    let instruction =
        match op with
        | Add -> sprintf "%s = add i32 %s, %s" resultReg left.Name right.Name
        | Subtract -> sprintf "%s = sub i32 %s, %s" resultReg left.Name right.Name
        | Multiply -> sprintf "%s = mul i32 %s, %s" resultReg left.Name right.Name
        | Divide -> sprintf "%s = sdiv i32 %s, %s" resultReg left.Name right.Name
        | Equal -> 
            sprintf "%s = icmp eq i32 %s, %s" resultReg left.Name right.Name
        | NotEqual -> 
            sprintf "%s = icmp ne i32 %s, %s" resultReg left.Name right.Name
        | LessThan -> 
            sprintf "%s = icmp slt i32 %s, %s" resultReg left.Name right.Name
        | GreaterThan -> 
            sprintf "%s = icmp sgt i32 %s, %s" resultReg left.Name right.Name
    
    let resultType = 
        match op with
        | Add | Subtract | Multiply | Divide -> I32
        | Equal | NotEqual | LessThan | GreaterThan -> I1
    
    let newCtx = emit ctx1 instruction
    (newCtx, { Type = resultType; Name = resultReg })

// Generate LLVM IR for an expression
let rec generateExpression (ctx: CompilationContext) (expr: Expression) =
    match expr with
    | Literal lit -> 
        let (newCtx, typ, valStr) = generateLiteral ctx lit
        (newCtx, { Type = typ; Name = valStr })
        
    | Variable name ->
        match Map.tryFind name ctx.Variables with
        | Some value -> 
            // Load the variable from memory (alloca)
            let (newCtx, resultReg) = getNextRegister ctx
            let loadInstr = sprintf "%s = load %s, %s* %s" 
                                   resultReg 
                                   (llvmTypeToString value.Type) 
                                   (llvmTypeToString value.Type) 
                                   value.Name
            let newCtx2 = emit newCtx loadInstr
            (newCtx2, { Type = value.Type; Name = resultReg })
        | None -> 
            failwith (sprintf "Undefined variable: %s" name)
        
    | BinaryOperation(left, op, right) ->
        let (ctx1, leftValue) = generateExpression ctx left
        let (ctx2, rightValue) = generateExpression ctx1 right
        generateBinaryOp ctx2 leftValue op rightValue
        
    | FunctionCall(name, args) ->
        // Generate code for each argument
        let mutable currentCtx = ctx
        let argValues = ResizeArray<LLVMValue>()
        
        for arg in args do
            let (newCtx, argValue) = generateExpression currentCtx arg
            currentCtx <- newCtx
            argValues.Add(argValue)
        
        // Generate the function call
        let (newCtx, resultReg) = getNextRegister currentCtx
        
        let argStr = String.Join(", ", 
                                argValues |> Seq.map (fun arg -> 
                                    sprintf "%s %s" (llvmTypeToString arg.Type) arg.Name))
        
        let callInstr = 
            if name = "print" then
                // Handle print as a special case (call to printf)
                sprintf "%s = call i32 @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str.printf, i32 0, i32 0), i32 %s)" 
                       resultReg (argValues.[0].Name)
            else
                sprintf "%s = call i32 @%s(%s)" resultReg name argStr
        
        let newCtx2 = emit newCtx callInstr
        (newCtx2, { Type = I32; Name = resultReg })
        
    | Assignment(name, value) ->
        match Map.tryFind name ctx.Variables with
        | Some variable ->
            // Generate code for the value
            let (ctx1, valueResult) = generateExpression ctx value
            
            // Store the value in the variable
            let storeInstr = sprintf "store %s %s, %s* %s" 
                                   (llvmTypeToString valueResult.Type) 
                                   valueResult.Name 
                                   (llvmTypeToString variable.Type) 
                                   variable.Name
            
            let newCtx = emit ctx1 storeInstr
            (newCtx, valueResult)
        | None ->
            failwith (sprintf "Assignment to undefined variable: %s" name)

// Generate LLVM IR for variable allocation
let allocateVariable (ctx: CompilationContext) (name: string) (typ: LLVMType) =
    let (ctx1, varReg) = getNextRegister ctx
    
    // Allocate space for the variable on the stack
    let allocaInstr = sprintf "%s = alloca %s" varReg (llvmTypeToString typ)
    let newCtx = emit ctx1 allocaInstr
    
    // Add the variable to the context
    let varValue = { Type = typ; Name = varReg }
    let newCtx2 = { newCtx with Variables = Map.add name varValue newCtx.Variables }
    
    (newCtx2, varValue)

// Generate LLVM IR for a statement
let rec generateStatement (ctx: CompilationContext) (stmt: Statement) =
    match stmt with
    | ExpressionStatement expr ->
        let (newCtx, _) = generateExpression ctx expr
        newCtx
            
    | VariableDeclaration(name, initialValueOpt) ->
        // Allocate space for variable
        let (ctx1, varValue) = allocateVariable ctx name I32
        
        match initialValueOpt with
        | None -> ctx1
        | Some initialValue ->
            // Generate the initial value
            let (ctx2, valueResult) = generateExpression ctx1 initialValue
            
            // Store the value in the variable
            let storeInstr = sprintf "store %s %s, %s* %s" 
                                   (llvmTypeToString valueResult.Type) 
                                   valueResult.Name 
                                   (llvmTypeToString varValue.Type) 
                                   varValue.Name
            
            emit ctx2 storeInstr
                
    | IfStatement(condition, thenBlock, elseBlockOpt) ->
        let (ctx1, condValue) = generateExpression ctx condition
        
        // Create labels for then, else and end blocks
        let (ctx2, thenLabel) = getNextLabel ctx1 "if.then."
        let (ctx3, endLabel) = getNextLabel ctx2 "if.end."
        
        match elseBlockOpt with
        | None ->
            // Generate branch instruction
            let branchInstr = sprintf "br i1 %s, label %%%s, label %%%s" condValue.Name thenLabel endLabel
            let ctx4 = emit ctx3 branchInstr
            
            // Generate then block
            let ctx5 = emit ctx4 (thenLabel + ":")
            let ctx6 = generateBlock ctx5 thenBlock
            
            // Branch to end block after then block
            let ctx7 = emit ctx6 (sprintf "br label %%%s" endLabel)
            
            // Generate end block
            emit ctx7 (endLabel + ":")
            
        | Some elseBlock ->
            let (ctx4, elseLabel) = getNextLabel ctx3 "if.else."
            
            // Generate branch instruction
            let branchInstr = sprintf "br i1 %s, label %%%s, label %%%s" condValue.Name thenLabel elseLabel
            let ctx5 = emit ctx4 branchInstr
            
            // Generate then block
            let ctx6 = emit ctx5 (thenLabel + ":")
            let ctx7 = generateBlock ctx6 thenBlock
            
            // Branch to end block after then block
            let ctx8 = emit ctx7 (sprintf "br label %%%s" endLabel)
            
            // Generate else block
            let ctx9 = emit ctx8 (elseLabel + ":")
            let ctx10 = generateBlock ctx9 elseBlock
            
            // Branch to end block after else block
            let ctx11 = emit ctx10 (sprintf "br label %%%s" endLabel)
            
            // Generate end block
            emit ctx11 (endLabel + ":")
                
    | WhileStatement(condition, body) ->
        // Create labels for condition, body and end blocks
        let (ctx1, condLabel) = getNextLabel ctx "while.cond."
        let (ctx2, bodyLabel) = getNextLabel ctx1 "while.body."
        let (ctx3, endLabel) = getNextLabel ctx2 "while.end."
        
        // Branch to condition block
        let ctx4 = emit ctx3 (sprintf "br label %%%s" condLabel)
        
        // Generate condition block
        let ctx5 = emit ctx4 (condLabel + ":")
        let (ctx6, condValue) = generateExpression ctx5 condition
        
        // Branch based on condition
        let branchInstr = sprintf "br i1 %s, label %%%s, label %%%s" condValue.Name bodyLabel endLabel
        let ctx7 = emit ctx6 branchInstr
        
        // Generate body block
        let ctx8 = emit ctx7 (bodyLabel + ":")
        let ctx9 = generateBlock ctx8 body
        
        // Branch back to condition block after body
        let ctx10 = emit ctx9 (sprintf "br label %%%s" condLabel)
        
        // Generate end block
        emit ctx10 (endLabel + ":")
            
    | FunctionDeclaration(name, parameters, body) ->
        // Finalize any existing function
        let ctx1 = finalizeFunction ctx
        
        // Create parameter types (all i32 for simplicity)
        let paramTypes = parameters |> List.map (fun _ -> I32)
        
        // Create the function declaration
        let ctx2 = addFunctionDeclaration ctx1 name paramTypes I32
        
        // Allocate space for parameters
        let mutable currentCtx = ctx2
        
        // Add parameters to the variable map
        for (i, param) in List.indexed parameters do
            // Allocate space for the parameter
            let (newCtx, varValue) = allocateVariable currentCtx param I32
            currentCtx <- newCtx
            
            // Store the parameter value in the allocated space
            let paramReg = sprintf "%%%d" (i + 1)  // LLVM uses %1, %2, etc. for parameters
            let storeInstr = sprintf "store i32 %s, i32* %s" paramReg varValue.Name
            currentCtx <- emit currentCtx storeInstr
        
        // Generate code for the function body
        let ctx3 = generateBlock currentCtx body
        
        // Add a default return if none is present
        let ctx4 = emit ctx3 "ret i32 0"
        
        // Finalize the function
        finalizeFunction ctx4
            
    | ReturnStatement valueOpt ->
        match valueOpt with
        | None ->
            // Return void
            emit ctx "ret void"
        | Some value ->
            // Generate the return value
            let (ctx1, valueResult) = generateExpression ctx value
            
            // Return the value
            let retInstr = sprintf "ret %s %s" (llvmTypeToString valueResult.Type) valueResult.Name
            emit ctx1 retInstr

// Generate LLVM IR for a block of statements
and generateBlock (ctx: CompilationContext) (block: Statement list) =
    // Generate code for each statement in the block
    let mutable currentCtx = ctx
    
    for stmt in block do
        currentCtx <- generateStatement currentCtx stmt
        
    currentCtx

// Generate LLVM IR for an entire program
let generateCode (program: Program) : CompilationResult<string> =
    try
        let ctx = createContext()
        
        // Add printf declaration for the print function
        ctx.Module.AppendLine("declare i32 @printf(i8*, ...)") |> ignore
        ctx.Module.AppendLine("@.str.printf = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"") |> ignore
        ctx.Module.AppendLine() |> ignore
        
        // Define the print function
        let printFuncCtx = addFunctionDeclaration ctx "print" [I32] I32
        let printFuncCtx2 = emit printFuncCtx "%1 = load i32, i32* %0"
        let printFuncCtx3 = emit printFuncCtx2 "%2 = call i32 @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str.printf, i32 0, i32 0), i32 %1)"
        let printFuncCtx4 = emit printFuncCtx3 "ret i32 0"
        let ctx2 = finalizeFunction printFuncCtx4
        
        // Generate code for the program
        let ctx3 = generateBlock ctx2 program
        let ctx4 = finalizeFunction ctx3
        
        // Add main function if not defined
        if not (program |> List.exists (function 
                | FunctionDeclaration(name, _, _) when name = "main" -> true 
                | _ -> false)) then
            let mainFuncCtx = addFunctionDeclaration ctx4 "main" [] I32
            let mainFuncCtx2 = emit mainFuncCtx "ret i32 0"
            let ctx5 = finalizeFunction mainFuncCtx2
            Success (ctx5.Module.ToString())
        else
            Success (ctx4.Module.ToString())
    with
    | ex -> Error { Message = sprintf "Code generation error: %s" ex.Message; Statement = None }