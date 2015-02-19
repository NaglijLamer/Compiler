# Compiler
Compiler from C-like language to byte-code. 

# Grammar of language.
Program := StatementList <br />
Body := "{" StatementList "}" <br />
StatementList := Statement+ <br />
Statement := Declaration | Assignment | ReturnSt | FunctionCall ";" | WhileLoop | ForLoop | IfElse | Function <br />
Function := FunctionHeader FunctionBody <br />
FunctionBody := "native" "'" Name "'" ";" | Body <br />
FunctionHeader := "function" TypeF Name "("ArgsListS")" <br />
ArgListS := (ArgS ("," ArgS)*)? <br />
ArgS := Type Name <br />
Type := "int" | "double" | "string" <br />
TypeF := Type | "void" <br />
ReturnSt := "return" Expression ";" <br />
Declaration := Type Name ";" <br />
Assignment := Name "=" Expression ";" <br />
ForLoop := "for" "(" Name "in" (Name | Number) ".." (Name | Number) ")" Body <br />
WhileLoop := "while" "(" Expression ")" Body <br />
IfElse := "if" "(" Expression ")" Body ["else" Body] <br />
FunctionCall := Name "(" ArgsListC ")" <br />
ArgListC := (Expression ("," Expression)*)? <br />
Expression := ("-" | "!")? SubExpr0 <br />
SubExpr0 := SubExpr1 OperBool Expression | SubExpr1 <br />
SubExpr1 := SubExpr2 OperEq Expression | SubExpr2 <br />
SubExpr2 := SubExpr3 OperOrd Expression | SubExpr3 <br />
SubExpr3 := SubExpr4 OperLow Expression | SubExpr4 <br />
SubExpr4 := SubExpr5 OperHigh Expression | SubExpr5 <br />
SubExpr5 := "(" Expression ")" | Number | Name | String | FunctionCall <br />
OperOrd := "<" | "<=" | ">" | ">=" <br />
OperBool := "||" | "&&" <br />
OperEq := "==" | "!=" <br />
OperLow := "+" | "-" <br />
OperHigh := "*" | "/" <br />
