module Parser
(	functHeadParser
,	functBodyParser
,	statementParser
,	whileParser
,	forParser
,	ifParser
,	bodyParser
,	assignmentParser
,	declarationParser
,	returnParser
,	functionCallParser
,	boolParser
,	expressionParser
,	mainParser
) where

import Prelude
import Control.Monad
import Control.Applicative
import Text.Parsec
-- some another imports

expressionParser :: CharParser () String

whileParser :: CharParser () [String]

forParser :: CharParser () [String]

mainParser :: String -> Either ParseError [[String]]
mainParser inp = parse functBodyParser "(unknown)" inp

