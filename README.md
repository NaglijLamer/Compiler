# Compiler
Compiler from C-like language to byte-code. 

# Grammar of language.
Program := StatementList 
Body := "{" StatementList "}" 
StatementList := Statement+ 
Statement := Declaration | Assignment | ReturnSt | FunctionCall ";" | WhileLoop | ForLoop | IfElse | Function 
Function := FunctionHeader FunctionBody 
FunctionBody := "native" "'" Name "'" ";" | Body 
FunctionHeader := "function" TypeF Name "("ArgsListS")" 
ArgListS := (ArgS ("," ArgS)*)? 
ArgS := Type Name 
Type := "int" | "double" | "string" 
TypeF := Type | "void" 
ReturnSt := "return" Expression ";" 
Declaration := Type Name ";" 
Assignment := Name "=" Expression ";" 
ForLoop := "for" "(" Name "in" (Name | Number) ".." (Name | Number) ")" Body 
WhileLoop := "while" "(" Expression ")" Body 
IfElse := "if" "(" Expression ")" Body ["else" Body] 
FunctionCall := Name "(" ArgsListC ")" 
ArgListC := (Expression ("," Expression)*)? 
Expression := ("-" | "!")? SubExpr0 
SubExpr0 := SubExpr1 OperBool Expression | SubExpr1 
SubExpr1 := SubExpr2 OperEq Expression | SubExpr2 
SubExpr2 := SubExpr3 OperOrd Expression | SubExpr3 
SubExpr3 := SubExpr4 OperLow Expression | SubExpr4 
SubExpr4 := SubExpr5 OperHigh Expression | SubExpr5 
SubExpr5 := "(" Expression ")" | Number | Name | String | FunctionCall 
OperOrd := "<" | "<=" | ">" | ">=" 
OperBool := "||" | "&&" 
OperEq := "==" | "!=" 
OperLow := "+" | "-" 
OperHigh := "*" | "/" 
