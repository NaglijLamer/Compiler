# Compiler
Compiler from C-like language to byte-code. 

# Grammar of language.
Program := StatementList <br />
Body := "{" StatementList "}" <br />
StatementList := Statement+ <br />
Statement := Declaration | Assignment | ReturnSt | PrintSt | FunctionCall ";" | WhileLoop | ForLoop | IfElse | Function <br />
Function := FunctionHeader FunctionBody <br />
FunctionBody := "native" "'" Name "'" ";" | Body <br />
FunctionHeader := "function" TypeF Name "("ArgsListS")" <br />
ArgListS := (ArgS ("," ArgS)\*)? <br />
ArgS := Type Name <br />
Type := "int" | "double" | "string" <br />
TypeF := Type | "void" <br />
ReturnSt := "return" [Expression] ";" <br />
PrintSt := "print" "(" Expression* ")" ";" <br />
Declaration := Type Name ";" <br />
Assignment := Name "=" Expression ";" <br />
ForLoop := "for" "(" Name "in" (Name | Number) ".." (Name | Number) ")" Body <br />
WhileLoop := "while" "(" Expression ")" Body <br />
IfElse := "if" "(" Expression ")" Body ["else" Body] <br />
FunctionCall := Name "(" ArgsListC ")" <br />
ArgListC := (Expression ("," Expression)\*)? <br />
Expression := ("-" | "!")? SubExpr0 <br />
SubExpr0 := Expression OperBool SubExpr1 | SubExpr1 <br />
SubExpr1 := Expression OperEq SubExpr2 | SubExpr2 <br />
SubExpr2 := Expression OperOrd SubExpr3 | SubExpr3 <br />
SubExpr3 := Expression OperLow SubExpr4 | SubExpr4 <br />
SubExpr4 := Expression OperHigh SubExpr5 | SubExpr5 <br />
SubExpr5 := "(" Expression ")" | Number | Name | String | FunctionCall <br />
OperOrd := "<" | "<=" | ">" | ">=" <br />
OperBool := "||" | "&&" <br />
OperEq := "==" | "!=" <br />
OperLow := "+" | "-" <br />
OperHigh := "*" | "/" <br />
