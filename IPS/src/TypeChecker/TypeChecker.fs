module TypeChecker

open System.Collections.Generic
open AST

type Type =
    | IntType
    | StringType
    | BoolType
    | FunctionType of Type list * Type
    | VoidType
    | UnknownType

type TypedExpression = {
    Expression: Expression
    Type: Type
}

type Symbol = {
    Name: string
    Type: Type
    IsMutable: bool
}

type SymbolTable = Dictionary<string, Symbol>

type TypeEnvironment = {
    Variables: SymbolTable
    Functions: SymbolTable
    Parent: TypeEnvironment option
    ReturnType: Type option
}

type TypeCheckError = {
    Message: string
    Expression: Expression option
    Statement: Statement option
}

type TypeCheckResult<'a> =
    | Success of 'a
    | Error of TypeCheckError

// Create a new type environment with optional parent
let createEnvironment (parent: TypeEnvironment option) (returnType: Type option) : TypeEnvironment =
    {
        Variables = new SymbolTable()
        Functions = new SymbolTable()
        Parent = parent
        ReturnType = returnType
    }

// Look up a variable in the environment (including parent scopes)
let rec lookupVariable (env: TypeEnvironment) (name: string) : Symbol option =
    if env.Variables.ContainsKey(name) then
        Some env.Variables.[name]
    else
        match env.Parent with
        | Some parent -> lookupVariable parent name
        | None -> None

// Look up a function in the environment (including parent scopes)
let rec lookupFunction (env: TypeEnvironment) (name: string) : Symbol option =
    if env.Functions.ContainsKey(name) then
        Some env.Functions.[name]
    else
        match env.Parent with
        | Some parent -> lookupFunction parent name
        | None -> None

// Add a variable to the current environment
let addVariable (env: TypeEnvironment) (name: string) (typ: Type) (isMutable: bool) : unit =
    if env.Variables.ContainsKey(name) then
        failwith (sprintf "Variable '%s' already defined in this scope" name)
    else
        env.Variables.Add(name, { Name = name; Type = typ; IsMutable = isMutable })

// Add a function to the current environment
let addFunction (env: TypeEnvironment) (name: string) (paramTypes: Type list) (returnType: Type) : unit =
    if env.Functions.ContainsKey(name) then
        failwith (sprintf "Function '%s' already defined in this scope" name)
    else
        env.Functions.Add(name, { 
            Name = name; 
            Type = FunctionType(paramTypes, returnType); 
            IsMutable = false 
        })

// Infer the type of a literal value
let inferLiteralType (literal: Literal) : Type =
    match literal with
    | IntLiteral _ -> IntType
    | StringLiteral _ -> StringType
    | BoolLiteral _ -> BoolType

// Get type of binary operator result based on operand types
let getBinaryOpResultType (op: BinaryOp) (leftType: Type) (rightType: Type) : TypeCheckResult<Type> =
    match op, leftType, rightType with
    | Add, IntType, IntType -> Success IntType
    | Subtract, IntType, IntType -> Success IntType
    | Multiply, IntType, IntType -> Success IntType
    | Divide, IntType, IntType -> Success IntType
    | Add, StringType, StringType -> Success StringType  // String concatenation
    | Equal, _, _ when leftType = rightType -> Success BoolType
    | NotEqual, _, _ when leftType = rightType -> Success BoolType
    | LessThan, IntType, IntType -> Success BoolType
    | GreaterThan, IntType, IntType -> Success BoolType
    | _ -> Error { 
        Message = sprintf "Invalid operand types for binary operator: %A and %A" leftType rightType
        Expression = None
        Statement = None 
    }

// Type check an expression
let rec typeCheckExpression (env: TypeEnvironment) (expr: Expression) : TypeCheckResult<TypedExpression> =
    match expr with
    | Literal lit -> 
        Success { Expression = expr; Type = inferLiteralType lit }
        
    | Variable name ->
        match lookupVariable env name with
        | Some symbol -> Success { Expression = expr; Type = symbol.Type }
        | None -> Error { 
            Message = sprintf "Undefined variable: %s" name
            Expression = Some expr
            Statement = None 
        }
        
    | BinaryOperation(left, op, right) ->
        match typeCheckExpression env left with
        | Error e -> Error e
        | Success leftTyped ->
            match typeCheckExpression env right with
            | Error e -> Error e
            | Success rightTyped ->
                match getBinaryOpResultType op leftTyped.Type rightTyped.Type with
                | Error e -> Error e
                | Success resultType ->
                    Success { Expression = expr; Type = resultType }
                    
    | FunctionCall(fname, args) ->
        match lookupFunction env fname with
        | None -> Error { 
            Message = sprintf "Undefined function: %s" fname
            Expression = Some expr
            Statement = None 
        }
        | Some symbol ->
            match symbol.Type with
            | FunctionType(paramTypes, returnType) ->
                if paramTypes.Length <> List.length args then
                    Error { 
                        Message = sprintf "Function '%s' expects %d arguments but got %d" fname paramTypes.Length (List.length args)
                        Expression = Some expr
                        Statement = None 
                    }
                else
                    let mutable argsValid = true
                    let mutable errorRes = None
                    
                    // Check each argument
                    for i in 0 .. (List.length args) - 1 do
                        if argsValid then
                            let arg = List.item i args
                            match typeCheckExpression env arg with
                            | Error e -> 
                                argsValid <- false
                                errorRes <- Some e
                            | Success argTyped ->
                                if argTyped.Type <> paramTypes.[i] && paramTypes.[i] <> UnknownType then
                                    argsValid <- false
                                    errorRes <- Some {
                                        Message = sprintf "Argument %d of '%s' has wrong type (expected %A, got %A)" (i+1) fname paramTypes.[i] argTyped.Type
                                        Expression = Some arg
                                        Statement = None
                                    }
                    
                    if argsValid then
                        Success { Expression = expr; Type = returnType }
                    else
                        match errorRes with
                        | Some errorV -> Error errorV
                        | None -> 
                            Error {
                                Message = "Unknown error in function call"
                                Expression = Some expr
                                Statement = None
                            }
            | _ -> 
                Error { 
                    Message = sprintf "'%s' is not a function" fname
                    Expression = Some expr
                    Statement = None 
                }
                
    | Assignment(vname, vexpr) ->
        match lookupVariable env vname with
        | None -> Error { 
            Message = sprintf "Cannot assign to undefined variable: %s" vname
            Expression = Some expr
            Statement = None 
        }
        | Some symbol ->
            if not symbol.IsMutable then
                Error { 
                    Message = sprintf "Cannot assign to immutable variable: %s" vname
                    Expression = Some expr
                    Statement = None 
                }
            else
                match typeCheckExpression env vexpr with
                | Error e -> Error e
                | Success valueTyped ->
                    if valueTyped.Type <> symbol.Type && symbol.Type <> UnknownType then
                        Error { 
                            Message = sprintf "Cannot assign value of type %A to variable '%s' of type %A" valueTyped.Type vname symbol.Type
                            Expression = Some expr
                            Statement = None 
                        }
                    else
                        Success { Expression = expr; Type = symbol.Type }

// Type check a statement
let rec typeCheckStatement (env: TypeEnvironment) (stmt: Statement) : TypeCheckResult<unit> =
    match stmt with
    | ExpressionStatement expr ->
        match typeCheckExpression env expr with
        | Error e -> Error e
        | Success _ -> Success ()
        
    | VariableDeclaration(name, initialValueOpt) ->
        match initialValueOpt with
        | None -> 
            // Default to unknown type if no initial value
            addVariable env name UnknownType true
            Success ()
        | Some initialValue ->
            match typeCheckExpression env initialValue with
            | Error e -> Error e
            | Success typedValue ->
                addVariable env name typedValue.Type true
                Success ()
                
    | IfStatement(condition, thenBlock, elseBlockOpt) ->
        match typeCheckExpression env condition with
        | Error e -> Error e
        | Success conditionTyped ->
            if conditionTyped.Type <> BoolType then
                Error { 
                    Message = "If condition must be a boolean expression"
                    Expression = Some condition
                    Statement = Some stmt 
                }
            else
                match typeCheckBlock env thenBlock with
                | Error e -> Error e
                | Success _ ->
                    match elseBlockOpt with
                    | None -> Success ()
                    | Some elseBlock ->
                        match typeCheckBlock env elseBlock with
                        | Error e -> Error e
                        | Success _ -> Success ()
                        
    | WhileStatement(condition, body) ->
        match typeCheckExpression env condition with
        | Error e -> Error e
        | Success conditionTyped ->
            if conditionTyped.Type <> BoolType then
                Error { 
                    Message = "While condition must be a boolean expression"
                    Expression = Some condition
                    Statement = Some stmt 
                }
            else
                match typeCheckBlock env body with
                | Error e -> Error e
                | Success _ -> Success ()
                
    | FunctionDeclaration(name, parameters, body) ->
        // Create a function symbol with unknown parameter types for now
        let paramTypes = List.map (fun _ -> UnknownType) parameters
        addFunction env name paramTypes VoidType
        
        // Create a new environment for the function body with the return type
        let functionEnv = createEnvironment (Some env) (Some VoidType)
        
        // Add parameters to the function environment
        for param in parameters do
            addVariable functionEnv param UnknownType false
            
        // Type check the function body
        match typeCheckBlock functionEnv body with
        | Error e -> Error e
        | Success _ -> Success ()
        
    | ReturnStatement valueOpt ->
        match env.ReturnType with
        | None -> 
            Error { 
                Message = "Return statement outside of function"
                Expression = None
                Statement = Some stmt 
            }
        | Some expectedType ->
            match valueOpt with
            | None ->
                if expectedType <> VoidType then
                    Error { 
                        Message = sprintf "Function must return a value of type %A" expectedType
                        Expression = None
                        Statement = Some stmt 
                    }
                else
                    Success ()
            | Some value ->
                match typeCheckExpression env value with
                | Error e -> Error e
                | Success valueTyped ->
                    if valueTyped.Type <> expectedType && expectedType <> UnknownType then
                        Error { 
                            Message = sprintf "Function should return %A but returns %A" expectedType valueTyped.Type
                            Expression = Some value
                            Statement = Some stmt 
                        }
                    else
                        Success ()

// Type check a block of statements
and typeCheckBlock (env: TypeEnvironment) (block: Statement list) : TypeCheckResult<unit> =
    // Create a new scope for the block
    let blockEnv = createEnvironment (Some env) env.ReturnType
    
    // Type check each statement in the block
    let mutable result = Success ()
    for stmt in block do
        match result with
        | Error _ -> ()
        | Success _ ->
            result <- typeCheckStatement blockEnv stmt
            
    result

// Type check an entire program
let typeCheck (program: Program) : TypeCheckResult<Program> =
    // Create the global environment
    let globalEnv = createEnvironment None None
    
    // Add built-in functions
    addFunction globalEnv "print" [UnknownType] VoidType
    
    // Type check each statement in the program
    match typeCheckBlock globalEnv program with
    | Error e -> Error e
    | Success _ -> Success program