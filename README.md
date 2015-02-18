# Compiler
Compiler from C-like language to byte-code.

# Grammar of language.
Body = "{" StatementList "}"
StatementList = Statement [StatementList]
Statement = Declaration | Assignment | ReturnExpression | FunctionCall ";" | WhileLoop | ForLoop | IfElse | Function
Function = FunctionHeader FunctionBody
FunctionBody = "native" "'" Name "'" ";" | Body
FunctionHeader = "function" TypeF Name "("ArgsS")"
ArgsS = eps | ArglistS
ArglistS = ArgS ["," ArgListS]
ArgS = Type Name
Type = "int" | "double" | "string"
TypeF = Type | "void"
ReturnExpression = "return" Expression ";"
Declaration = Type Name ";"
Assignment = Name "=" Expression ";"
ForLoop = "for" "(" Name in Number ".." Number ")" Body
WhileLoop = "while" "(" BoolExpression ")" Body
IfElse = "if" "(" BoolExpression ")" Body ["else" Body]
BoolExpression = ["!"] SubBoolExpr
SubBoolExpr = "(" BoolExpression ")" | SubBoolExpr BoolOp SubBoolExpr | Bool
Bool = Expression Ord Expression
BoolOp = "||" | "&&"
Ord = "==" | "!=" || "<" || "<=" || ">" || ">="
FunctionCall = Name "(" ArgsC ")"
ArgsC = eps | ArgListC
ArglistC = Expression ["," ArgListC]
Expression = ["-"] SubExpr
SubExpr = "(" Expression ")" | Name | String | Number | FunctionCall | SubExpr Oper SubExpr
Oper = "+" | "-" | "*" | "/"
Name = Letter [Letter | "_"]
Number = ["-"] {Num} ["."{Num}]
String = {Letter | Number | "\"}
