module Lexer

type Position = { Line: int; Column: int }

type Token =
    | Identifier of string
    | Integer of int
    | String of string
    | Operator of string
    | Keyword of string
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Semicolon
    | Comma
    | EOF
    | Unknown of string

type TokenInfo = {
    Token: Token
    Position: Position
}

let isWhitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let isDigit c = c >= '0' && c <= '9'
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isAlphaNumeric c = isAlpha c || isDigit c

let keywords = [
    "function"; "var"; "return"; "if"; "else"; "while"; "for"; "print"
]

let rec tokenizeRec (source: string) (pos: int) (line: int) (col: int) (tokens: TokenInfo list) =
    if pos >= source.Length then
        List.rev ({ Token = EOF; Position = { Line = line; Column = col } } :: tokens)
    else
        let c = source.[pos]
        let currentPos = { Line = line; Column = col }
        
        match c with
        | c when isWhitespace c ->
            let newLine, newCol = 
                if c = '\n' then line + 1, 1
                else line, col + 1
            tokenizeRec source (pos + 1) newLine newCol tokens
            
        | c when isDigit c ->
            let rec readNumber idx acc =
                if idx < source.Length && isDigit source.[idx] then
                    readNumber (idx + 1) (acc * 10 + (int source.[idx] - int '0'))
                else
                    idx, acc
            
            let endPos, value = readNumber pos 0
            let token = { Token = Integer value; Position = currentPos }
            tokenizeRec source endPos line (col + (endPos - pos)) (token :: tokens)
            
        | c when isAlpha c || c = '_' ->
            let rec readIdentifier idx acc =
                if idx < source.Length && (isAlphaNumeric source.[idx] || source.[idx] = '_') then
                    readIdentifier (idx + 1) (acc + string source.[idx])
                else
                    idx, acc
            
            let endPos, name = readIdentifier pos ""
            let token = 
                if List.contains name keywords then
                    { Token = Keyword name; Position = currentPos }
                else
                    { Token = Identifier name; Position = currentPos }
            
            tokenizeRec source endPos line (col + (endPos - pos)) (token :: tokens)
            
        | '(' -> 
            let token = { Token = LeftParen; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | ')' -> 
            let token = { Token = RightParen; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | '{' -> 
            let token = { Token = LeftBrace; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | '}' -> 
            let token = { Token = RightBrace; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | ';' -> 
            let token = { Token = Semicolon; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | ',' -> 
            let token = { Token = Comma; Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)
            
        | '"' ->
            let rec readString idx acc =
                if idx < source.Length && source.[idx] <> '"' then
                    readString (idx + 1) (acc + string source.[idx])
                elif idx < source.Length then
                    idx + 1, acc  // Skip closing quote
                else
                    idx, acc  // Unterminated string
            
            let endPos, value = readString (pos + 1) ""
            let token = { Token = String value; Position = currentPos }
            tokenizeRec source endPos line (col + (endPos - pos)) (token :: tokens)
            
        | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' ->
            // Check for two-character operators like ==, !=, <=, >=
            let nextPos = pos + 1
            let op = 
                if nextPos < source.Length && source.[nextPos] = '=' then
                    let op = string c + "="
                    { Token = Operator op; Position = currentPos }, pos + 2, col + 2
                else
                    { Token = Operator (string c); Position = currentPos }, pos + 1, col + 1
            
            let token, newPos, newCol = op
            tokenizeRec source newPos line newCol (token :: tokens)
            
        // Handle comments correctly
        | '/' when pos + 1 < source.Length && source.[pos + 1] = '/' ->
            // Comment: skip to end of line
            let rec skipComment idx =
                if idx < source.Length && source.[idx] <> '\n' then
                    skipComment (idx + 1)
                else
                    idx
            
            let endPos = skipComment pos
            tokenizeRec source endPos line col tokens
            
        | _ ->
            let token = { Token = Unknown (string c); Position = currentPos }
            tokenizeRec source (pos + 1) line (col + 1) (token :: tokens)

let tokenize (source: string) =
    tokenizeRec source 0 1 1 []
