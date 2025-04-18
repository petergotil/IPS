module AST

type Literal =
    | IntLiteral of int
    | StringLiteral of string
    | BoolLiteral of bool

type BinaryOp =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan

type Expression =
    | Literal of Literal
    | Variable of string
    | BinaryOperation of Expression * BinaryOp * Expression
    | FunctionCall of string * Expression list
    | Assignment of string * Expression

type Statement =
    | ExpressionStatement of Expression
    | VariableDeclaration of string * Expression option
    | IfStatement of Expression * Statement list * Statement list option
    | WhileStatement of Expression * Statement list
    | FunctionDeclaration of string * string list * Statement list
    | ReturnStatement of Expression option

type Program = Statement list