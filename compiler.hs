
module Main
(main)where

import Parser
import Translator
import Printer
import System.Environment 
import qualified Data.ByteString.Lazy as L

main = do { c <- getArgs
;	if ((length c < 3) && (length c) > 0) 
		then do { cont <- readFile (head c)
			; case mainParser cont of
				Left e -> do {
					putStrLn "There is a syntax error!"
					; print e }
				Right r -> do {
					case (length c) of
						1 -> L.writeFile ("./outp") (mainPrinter $ mainTranslator r)
						2 -> L.writeFile (last c) (mainPrinter $ mainTranslator r) } }
		else error ("Wrong arguments")}
