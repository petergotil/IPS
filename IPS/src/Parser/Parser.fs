module Parser

open Lexer
open AST

type ParseError = {
    Message: string
    Position: Position
}

type ParseResult<'a> =
    | Success of 'a
    | Error of ParseError

type Parser<'a> = TokenInfo list -> (TokenInfo list * 'a) option

let pReturn v : Parser<'a> = 
    fun tokens -> Some (tokens, v)

let pBind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun tokens ->
        match p tokens with
        | None -> None
        | Some (tokens', a) -> f a tokens'

let (>>=) = pBind

let pMap (f: 'a -> 'b) (p: Parser<'a>) : Parser<'b> =
    p >>= (f >> pReturn)

let (|>>) p f = pMap f p

let pZero : Parser<'a> = fun _ -> None

let pPlus (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    fun tokens ->
        match p1 tokens with
        | Some _ as result -> result
        | None -> p2 tokens

let (<|>) = pPlus

let rec pMany (p: Parser<'a>) : Parser<'a list> =
    fun tokens ->
        match p tokens with
        | None -> Some (tokens, [])
        | Some (tokens', a) ->
            match pMany p tokens' with
            | None -> Some (tokens', [a])
            | Some (tokens'', as') -> Some (tokens'', a :: as')

let pMany1 (p: Parser<'a>) : Parser<'a list> =
    p >>= (fun a -> pMany p |>> (fun as' -> a :: as'))

let satisfy (predicate: TokenInfo -> bool) : Parser<TokenInfo> =
    fun tokens ->
        match tokens with
        | t :: ts when predicate t -> Some (ts, t)
        | _ -> None

let token (t: Token) : Parser<TokenInfo> =
    satisfy (fun ti -> ti.Token = t)

let anyToken : Parser<TokenInfo> =
    fun tokens ->
        match tokens with
        | t :: ts -> Some (ts, t)
        | _ -> None

let keyword (k: string) : Parser<TokenInfo> =
    satisfy (fun ti -> 
        match ti.Token with
        | Token.Keyword kw when kw = k -> true
        | _ -> false)

let identifier : Parser<string> =
    satisfy (fun ti -> 
        match ti.Token with
        | Token.Identifier _ -> true
        | _ -> false) 
    |>> (fun ti -> 
        match ti.Token with
        | Token.Identifier name -> name
        | _ -> failwith "Not an identifier")

let integer : Parser<int> =
    satisfy (fun ti -> 
        match ti.Token with
        | Token.Integer _ -> true
        | _ -> false)
    |>> (fun ti -> 
        match ti.Token with
        | Token.Integer value -> value
        | _ -> failwith "Not an integer")

let stringLiteral : Parser<string> =
    satisfy (fun ti -> 
        match ti.Token with
        | Token.String _ -> true
        | _ -> false)
    |>> (fun ti -> 
        match ti.Token with
        | Token.String value -> value
        | _ -> failwith "Not a string")

let operator (op: string) : Parser<TokenInfo> =
    satisfy (fun ti -> 
        match ti.Token with
        | Token.Operator o when o = op -> true
        | _ -> false)

let leftParen = token Token.LeftParen
let rightParen = token Token.RightParen
let leftBrace = token Token.LeftBrace
let rightBrace = token Token.RightBrace
let semicolon = token Token.Semicolon
let comma = token Token.Comma

let between (p1: Parser<'a>) (p2: Parser<'b>) (p3: Parser<'c>) : Parser<'b> =
    p1 >>= (fun _ -> p2 >>= (fun b -> p3 >>= (fun _ -> pReturn b)))

// Forward declarations for recursive parsing
let expression, expressionImpl = 
    let r = ref (fun _ -> None) 
    (fun tokens -> !r tokens), r

let statement, statementImpl = 
    let r = ref (fun _ -> None) 
    (fun tokens -> !r tokens), r

let statementList, statementListImpl = 
    let r = ref (fun _ -> None) 
    (fun tokens -> !r tokens), r

// Parsing literal values
let parseLiteral : Parser<Expression> =
    (integer |>> (fun i -> Literal (IntLiteral i)))
    <|> (stringLiteral |>> (fun s -> Literal (StringLiteral s)))
    <|> (keyword "true" |>> (fun _ -> Literal (BoolLiteral true)))
    <|> (keyword "false" |>> (fun _ -> Literal (BoolLiteral false)))

// Parsing variables
let parseVariable : Parser<Expression> =
    identifier |>> (fun name -> Variable name)

// Parsing parenthesized expressions
let parseParenExpr : Parser<Expression> =
    between leftParen expression rightParen

// Parsing function calls
let parseFunctionCall : Parser<Expression> =
    identifier >>= (fun name ->
        between leftParen 
            (pMany (expression >>= (fun arg -> 
                (comma >>= (fun _ -> pReturn arg)) 
                <|> pReturn arg))) 
            rightParen
        |>> (fun args -> FunctionCall(name, args)))

// Parsing primary expressions
let parsePrimary : Parser<Expression> =
    parseLiteral <|> parseVariable <|> parseParenExpr <|> parseFunctionCall

// Parsing binary operations with precedence
let rec parseBinaryOp (operatorParser: Parser<BinaryOp>) (termParser: Parser<Expression>) : Parser<Expression> =
    termParser >>= (fun left ->
        (operatorParser >>= (fun op ->
            termParser >>= (fun right ->
                pReturn (BinaryOperation(left, op, right)))))
        <|> pReturn left)

// Parse multiplicative operators
let parseMultOp : Parser<BinaryOp> =
    (operator "*" |>> (fun _ -> Multiply))
    <|> (operator "/" |>> (fun _ -> Divide))

// Parse additive operators
let parseAddOp : Parser<BinaryOp> =
    (operator "+" |>> (fun _ -> Add))
    <|> (operator "-" |>> (fun _ -> Subtract))

// Parse comparison operators
let parseCompOp : Parser<BinaryOp> =
    (operator "==" |>> (fun _ -> Equal))
    <|> (operator "!=" |>> (fun _ -> NotEqual))
    <|> (operator "<" |>> (fun _ -> LessThan))
    <|> (operator ">" |>> (fun _ -> GreaterThan))

// Parse assignment expressions
let parseAssignment : Parser<Expression> =
    identifier >>= (fun name ->
        operator "=" >>= (fun _ ->
            expression |>> (fun expr ->
                Assignment(name, expr))))

// Implement expression parsing with operator precedence
do expressionImpl := 
    parseAssignment
    <|> (fun tokens ->
        let term1 = parseBinaryOp parseMultOp parsePrimary
        let term2 = parseBinaryOp parseAddOp term1
        let term3 = parseBinaryOp parseCompOp term2
        term3 tokens)

// Parse variable declarations
let parseVarDecl : Parser<Statement> =
    keyword "var" >>= (fun _ ->
        identifier >>= (fun name ->
            (operator "=" >>= (fun _ ->
                expression >>= (fun expr ->
                    semicolon >>= (fun _ ->
                        pReturn (VariableDeclaration(name, Some expr))))))
            <|>
            (semicolon >>= (fun _ ->
                pReturn (VariableDeclaration(name, None))))))

// Parse expression statements
let parseExprStmt : Parser<Statement> =
    expression >>= (fun expr ->
        semicolon >>= (fun _ ->
            pReturn (ExpressionStatement expr)))

// Parse return statements
let parseReturnStmt : Parser<Statement> =
    keyword "return" >>= (fun _ ->
        ((expression >>= (fun expr ->
            semicolon >>= (fun _ ->
                pReturn (ReturnStatement (Some expr))))))
        <|>
        (semicolon >>= (fun _ ->
            pReturn (ReturnStatement None))))

// Parse block of statements
let parseBlock : Parser<Statement list> =
    between leftBrace statementList rightBrace

// Parse if statements
let parseIfStmt : Parser<Statement> =
    keyword "if" >>= (fun _ ->
        between leftParen expression rightParen >>= (fun condition ->
            parseBlock >>= (fun thenBlock ->
                ((keyword "else" >>= (fun _ ->
                    parseBlock >>= (fun elseBlock ->
                        pReturn (IfStatement(condition, thenBlock, Some elseBlock))))))
                <|>
                pReturn (IfStatement(condition, thenBlock, None)))))

// Parse while statements
let parseWhileStmt : Parser<Statement> =
    keyword "while" >>= (fun _ ->
        between leftParen expression rightParen >>= (fun condition ->
            parseBlock >>= (fun body ->
                pReturn (WhileStatement(condition, body)))))

// Parse function declarations
let parseFunctionDecl : Parser<Statement> =
    keyword "function" >>= (fun _ ->
        identifier >>= (fun name ->
            between leftParen 
                (pMany (identifier >>= (fun param -> 
                    (comma >>= (fun _ -> pReturn param)) 
                    <|> pReturn param))) 
                rightParen
            >>= (fun parameters ->
                parseBlock >>= (fun body ->
                    pReturn (FunctionDeclaration(name, parameters, body))))))

// Implement statement parsing
do statementImpl := 
    parseFunctionDecl
    <|> parseVarDecl
    <|> parseIfStmt
    <|> parseWhileStmt
    <|> parseReturnStmt
    <|> parseExprStmt

// Implement statement list parsing
do statementListImpl := pMany statement

// Parse program (top level)
let parseProgram : Parser<Program> = statementList

// Parse full program from token list
let parse (tokens: TokenInfo list) : ParseResult<Program> =
    match parseProgram tokens with
    | Some (remaining, program) ->
        match remaining with
        | [] -> Success program
        | t :: _ -> Error { Message = "Unexpected tokens after valid program"; Position = t.Position }
    | None ->
        match tokens with
        | [] -> Error { Message = "Empty program"; Position = { Line = 0; Column = 0 } }
        | t :: _ -> Error { Message = "Failed to parse program"; Position = t.Position }