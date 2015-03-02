{-# LANGUAGE FlexibleContexts #-}
module Parser
(	
	mainParser
,	bodyParser
,	Expression (..)
,	Statement (..)
,	Binar (..)
,	Unar (..)
,	Name, Type, TypeF
) where

import Prelude
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec hiding (try)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- some another imports

languageDef = 
	emptyDef { Token.commentStart = "/*"
		, Token.commentEnd = "*/"
		, Token.commentLine = "//"
		, Token.identStart = letter
		, Token.identLetter = alphaNum
		, Token.reservedNames = [ "if"
					, "else"
					, "while"
					, "for"
					, "in"
					, "return"
					, "function"
					, "native"
					, "int"
					, "double"
					, "string"
					, "void"
					, ".."
					, "print"
					]
		, Token.reservedOpNames = [ "+", "-", "*", "/", "=", "!=", "<=", ">=", "<", ">", "!", "||", "&&"
					, "+=", "-=", "*=", "/=", "%", "%=", "&=", "|="
					]
		}
lexer = Token.makeTokenParser languageDef
identif = Token.identifier lexer
reserv = Token.reserved lexer
reservOp = Token.reservedOp lexer
paren = Token.parens lexer
integertok = Token.integer lexer
sem = Token.semi lexer
whiteSpaces = Token.whiteSpace lexer
brace = Token.braces lexer
nat = Token.natural lexer
flo = Token.float lexer
commSep = Token.commaSep lexer

type Name = String
type Type = String
type TypeF = String
data Val = N Name | I Int deriving (Show)
data Unar = Neg | Not deriving (Show)
data Binar =  And | Or | Eq | Gt | Lw | Gte | Lwe | Neq | Sum | Sub | Mult | Div | Mod deriving (Show)
instance Eq (Binar) where
	Sum == Sum = True
	Sub == Sub = True
	Mult == Mult = True
	Div == Div = True
	Mod == Mod = True
	Eq == Eq = True
	Neq == Neq = True
	Lw == Lw = True
	Gt == Gt = True
	Lwe == Lwe = True
	Gte == Gte = True
	And == And = True
	Or == Or = True
	_ == _ = False
data Expression = UnarExpr Unar Expression
		| BinaryExpr Binar Expression Expression
		| Ident Name
		| Str String
		-- | Numb String
		| IntNumb Integer
		| FloatNumb Double
		| FCall Name (Maybe [Expression]) deriving (Show)
data Statement = Declaration Type Name
		| Assignment Name Expression
		| PrintSt (Maybe [Expression])
		| ReturnSt Expression
		| FunctionCall Name (Maybe [Expression])
		| WhileLoop Expression [Statement]
		| ForLoop Name Expression Expression [Statement]
		| IfElse Expression [Statement] (Maybe [Statement])
		| Function TypeF Name (Maybe [Statement]) (Either Name [Statement]) deriving (Show)

-- data ASTree = St [Statement]
--	| Exp Expression deriving (Show)

--Unused
-- space1 :: Stream s m Char => ParsecT s u m Char
space1 = space <* whiteSpaces

-- Function for using code parser
mainParser :: String -> Either ParseError [Statement] 
mainParser inp = parse (bodyParser) "(unknown)" inp

-- Body of functions, loops, if-statements, e t.c.
bodyParser :: CharParser () [Statement]
bodyParser = many1 $ (whiteSpaces *> choice [ declarationParser <* sem
		,	printParser
		,	assignmentParser
		,	returnParser
		, 	whileLoopParser
		,	functionParser
		,	forLoopParser
		, 	ifElseParser
		]) 

-- Parsers for Statements
declarationParser :: CharParser () Statement
declarationParser = Declaration <$> typeParser <*> nameParser 

assignmentParser :: CharParser () Statement
--assignmentParser = nameParser >>= (\x -> assignmentTypeParser x >>= (\y -> sem >> return (Assignment x y)))
assignmentParser = nameParser >>= (\x -> (assignmentTypeParser x >>= (\y -> sem >> return (Assignment x y))) <|> (paren (cArgsParser) >>= (\y -> sem >> return (FunctionCall x y))))

assignmentTypeParser :: Name -> CharParser () Expression
assignmentTypeParser name = (reservOp "=" *> expressionParser <* whiteSpaces) <|> (opAssignment "+=" Sum) <|> (opAssignment "-=" Sub) <|> (opAssignment "*=" Mult) <|> (opAssignment "/=" Div) <|> (opAssignment "&=" And) <|> (opAssignment "|=" Or) <|> (opAssignment "%=" Mod) 
	where opAssignment tp op = BinaryExpr <$> (reservOp tp *> pure op) <*> (pure (Ident name) :: CharParser () Expression) <*> expressionParser <* whiteSpaces

printParser :: CharParser () Statement
printParser = PrintSt <$> (reserv "print" *> paren cArgsParser) <* sem

returnParser :: CharParser () Statement
returnParser = ReturnSt <$> (reserv "return" *> expressionParser) <* whiteSpaces <* sem

fCallParser :: CharParser () Expression 
fCallParser = FCall <$> nameParser <*> paren (cArgsParser)

whileLoopParser :: CharParser () Statement
whileLoopParser = WhileLoop <$> (reserv "while" *> paren expressionParser) <*> brace bodyParser

forLoopParser :: CharParser () Statement
--forLoopParser = reserv "for" *> paren (ForLoop <$> nameParser <* reserv "in" <*> expressionParser <* whiteSpaces <* string ".." <* whiteSpaces <*> expressionParser) <*> brace bodyParser
forLoopParser = reserv "for" *> paren (ForLoop <$> nameParser <* reserv "in" <*> (Ident <$> nameParser <|> IntNumb <$> nat) <* string ".." <* whiteSpaces <*> (Ident <$> nameParser <|> IntNumb <$> nat)) <*> brace bodyParser

ifElseParser :: CharParser () Statement
ifElseParser = IfElse <$> (reserv "if" *> paren expressionParser) <*> brace bodyParser <*> optionMaybe (reserv "else" *> brace bodyParser)

functionParser :: CharParser () Statement
functionParser = Function <$> (reserv "function" *> typeFParser) <*> nameParser <*> paren (sArgsParser) <*> functionBodyParser

functionBodyParser :: CharParser () (Either Name [Statement])
functionBodyParser = Left <$> (reserv "native" *> between (char '\'' <* spaces) (spaces *> char '\'') nameParser <* sem) <|> Right <$> brace bodyParser

typeParser :: CharParser () Type
typeParser = (reserv "double" >> return "double") <|> (reserv "int" >> return "int") <|> (reserv "string" >> return "string")

typeFParser :: CharParser () TypeF
typeFParser = typeParser <|> (reserv "void" >> return "void")

cArgsParser :: CharParser () (Maybe [Expression])
--cArgsParser = (++) <$> (pure <$> (whiteSpaces *> expressionParser <* whiteSpaces) :: CharParser () [Expression]) <*> (many (char ',' *> whiteSpaces *> expressionParser <* whiteSpaces))
cArgsParser = (commSep expressionParser >>= (\x -> if (null x) then (return Nothing) else return (Just x)))

sArgsParser :: CharParser () (Maybe [Statement])
--sArgsParser = (++) <$> (pure <$> (spaces *> declarationParser <* spaces) :: CharParser () [Statement]) <*> (many (char ',' *> spaces *> declarationParser <* spaces))
sArgsParser = (commSep declarationParser >>= (\x -> if (null x) then (return Nothing) else return (Just x)))

nameParser :: CharParser () Name
nameParser = identif

numberParser :: CharParser () String
--Maybe, that's very nice. 
numberParser = (++) <$> (many1 digit) <*> (option "" $ try((:) <$> char '.' <*> (many1 digit)))
--numberParser = (show <$> try(flo)) <|> (show <$> nat)

stringParser :: CharParser () String
--Maybe, that's nice.
stringParser = between (char '\'') (char '\'') (many (noneOf "\'"))

-- Parser for Expressions
unarOp op expr = Prefix (try (whiteSpaces >> string op >> whiteSpaces) >> return (UnarExpr expr))
binaryOp op expr = Infix (try (whiteSpaces >> string op >> whiteSpaces) >> return (BinaryExpr expr)) AssocLeft 
operators = 	[	[unarOp "-" Neg, unarOp "!" Not],
			[binaryOp "*" Mult, binaryOp "/" Div, binaryOp "%" Mod],
			[binaryOp "+" Sum, binaryOp "-" Sub],
			[binaryOp "<=" Lwe, binaryOp ">=" Gte, binaryOp "<" Lw, binaryOp ">" Gt],
			[binaryOp "==" Eq, binaryOp "!=" Neq],
			[binaryOp "&&" And],
			[binaryOp "||" Or]
		]

expressionParser :: CharParser () Expression
expressionParser = buildExpressionParser operators subExprParser

subExprParser :: CharParser () Expression
--subExprParser = paren expressionParser <|> try (fCallParser) <|> Ident <$> nameParser <|> Numb <$> numberParser <|> Str <$> stringParser
subExprParser = paren expressionParser <|> try (fCallParser) <|> Ident <$> nameParser <|> try (FloatNumb <$> flo) <|> IntNumb <$> nat <|> Str <$> stringParser

-- Test main function
main =
	do {c <- getContents
	;	case (parse bodyParser "stdin") c of 
			Left e -> do {
				putStrLn "There is a syntax error in your program!"
			;	print e }
			Right r -> mapM_ print r
	}
