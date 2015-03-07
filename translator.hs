
module Translator
(
	mainTranslator
,	bodyFunctTranslator
,	sizeofBC
,	ConstMap (..)
,	ByteCodeCommand (..)
,	SubBlock
,	Block (..)
,	Program (..)
) where

import Parser
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Control.Lens
import Control.Applicative

--Data type for byte code commands.
data ByteCodeCommand = DLOAD Double | ILOAD Int | SLOAD Int
			| DLOAD0 | ILOAD0 | SLOAD0
			| DLOAD1 | ILOAD1 | DLOADM1 | ILOADM1
			| DADD | IADD | DSUB | ISUB
			| DMUL | IMUL | DDIV | IDIV | IMOD
			| DNEG | INEG | IAOR | IAAND | IAXOR
			| IPRINT | DPRINT | SPRINT
			| I2D | D2I | S2I
			| SWAP | POP
			| LOADDVAR0 | LOADDVAR1 | LOADDVAR2 | LOADDVAR3
			| LOADIVAR0 | LOADIVAR1 | LOADIVAR2 | LOADIVAR3
			| LOADSVAR0 | LOADSVAR1 | LOADSVAR2 | LOADSVAR3
			| STOREIVAR0 | STOREIVAR1 | STOREIVAR2 | STOREIVAR3
			| STOREDVAR0 | STOREDVAR1 | STOREDVAR2 | STOREDVAR3
			| STORESVAR0 | STORESVAR1 | STORESVAR2 | STORESVAR3
			| LOADDVAR Int | LOADIVAR Int | LOADSVAR Int
			| STOREDVAR Int | STOREIVAR Int | STORESVAR Int
			| LOADCTXDVAR Int Int | LOADCTXIVAR Int Int
			| LOADCTXSVAR Int Int | STORECTXDVAR Int Int
			| STORECTXIVAR Int Int | STORECTXSVAR Int Int
			| DCMP | ICMP | JA Int | IFICMPNE Int
			| IFICMPE Int | IFICMPG Int | IFICMPGE Int
			| IFICMPL Int | IFICMPLE Int | DUMP
			| CALL Int | CALLNATIVE Int | RETURN | BREAK | STOP | INVALID deriving Show

--Main Function of translator.
mainTranslator :: [Statement] -> Program
mainTranslator = rootInitFunction

--Test main function.
main =
	do { c <- getContents
		;case (parse bodyParser "stdin") c of
			Left e -> do {
				putStrLn "There is a syntax error in your program!"
			;	print e }
			Right r -> do {
				mapM_ print r
			;	print (maxVar(fst(root(rootInitFunction r))))
			;	mapM_ print (Map.toList((varMap (fst(root (rootInitFunction r)))))) 
			;	mapM_ print (byteCode(fst(root(rootInitFunction r))))
			;	print (maxVar(bl(fst(root(rootInitFunction r))) !! 0))
			; 	mapM_ print (Map.toList((varMap (bl(fst(root(rootInitFunction r))) !! 0))))
			;	mapM_ print (byteCode(bl(fst(root(rootInitFunction r))) !! 0)) }}

type RetType = String
type Id = Int

--Data type for presentation of context tree.
data Block = Funct { name :: String
			, idf :: Id
			, retType :: RetType
			, tP :: (Maybe [Type])
			, byteCode :: [ByteCodeCommand]
			, varMap :: VarMap
			, maxVar :: Int 
			, bl :: [Block] }
		| IFLoop { bl :: [Block] } deriving Show

--Type for presentation of branch.
type SubBlock = (Block, [Int])

--Return block adressed by path.
getBlock :: SubBlock -> Maybe (Block)
getBlock (tree, []) = Just tree
getBlock (tree, path) = getBlock ((bl tree) !! (head path), (tail path))

--getBlock with Block -> [Int] instead of (Block, [Int]) as a SubBlock.
--getBlock' :: Block -> [Int] -> (Maybe Block)
--getBlock' tree [] = Just tree
--getBlock' tree path = getBlock ((bl tree) !! (head path) (tail path))

--Some functions for moving in tree.
moveUptoF :: SubBlock -> (Maybe SubBlock)
moveUptoF subbl = case (moveUp subbl) of
	Nothing -> Nothing
	Just (tree, path) -> case (getBlock (tree, path)) of
		Just (Funct _ _ _ _ _ _ _ _) -> Just (tree, path)
		Just (IFLoop _) -> moveUptoF (tree, path)
		Nothing -> error "Unexpected error"

moveUp :: SubBlock -> Maybe (SubBlock)
moveUp (tree, []) = Nothing
moveUp (tree, path) = Just (tree, init path)

--Types for constants, program, e t.c.
data LoopType = WHILE | FOR

type VarMap = Map.Map String (Id, Type)

type ConstMap = Map.Map String Id

data Program = Program { root :: SubBlock
			, countF :: Int
			, constantMap :: ConstMap
			, ast :: [Statement] } deriving Show

--Start function of translating. Create a Program and initiate root-function.
rootInitFunction :: [Statement] -> Program
rootInitFunction st = bodyFunctTranslator (Program ((Funct "0root" 0 "void" Nothing [] Map.empty 0 []), []) 1 Map.empty st)

--Translator for function body.
bodyFunctTranslator :: Program -> Program
--bodyFunctTranslator (Program (rt, path) cf cm []) = Program (rt, path) cf cm []
bodyFunctTranslator (Program (rt, []) cf cm []) = Program ((changeTreeNewBC rt [] [STOP]), []) cf cm []
bodyFunctTranslator (Program (rt, path) cf cm []) = Program ((isVoidFunct(rt, path)), path) cf cm []
bodyFunctTranslator (Program (rt, path) cf cm ((Declaration tp nm):xs)) = bodyFunctTranslator (Program ((declarationTranslator tp nm rt path), path) cf cm xs)
bodyFunctTranslator (Program (rt, path) cf cm ((Assignment nm expr):xs)) = bodyFunctTranslator (Program ((assignmentTranslator rt path cf cm nm expr), path) cf cm xs)
--For native function.
bodyFunctTranslator (Program (rt, path) cf cm ((Function rtp nm args (Left natnm)):xs)) = error ("Native functions aren't supported in this version of the compiler!")
bodyFunctTranslator (Program (rt, path) cf cm stm@((Function rtp nm args (Right body)):xs)) = bodyFunctTranslator (functionHeadTranslator (Program (rt, path) cf cm stm) (newPath (getBlock (rt, path)) path))
bodyFunctTranslator (Program (rt, path) cf cm ((ReturnSt stm):xs)) = bodyFunctTranslator (Program ((returnStTranslator (rt, path) stm cm), path) cf cm xs)
bodyFunctTranslator (Program (rt, path) cf cm ((PrintSt stm):xs)) = bodyFunctTranslator (Program ((printStTranslator (rt, path) stm cm), path) cf cm xs)
bodyFunctTranslator (Program (rt, path) cf cm stm@((WhileLoop expr body):xs)) = bodyFunctTranslator (whileLoopTranslator (Program ((changeTreeNewFunct rt path (IFLoop[])), path) cf cm stm) (newPath (getBlock (rt, path)) path) path)
bodyFunctTranslator (Program (rt, path) cf cm stm@((ForLoop nm expr1 expr2 body):xs)) = bodyFunctTranslator (forLoopTranslator (Program ((changeTreeNewFunct rt path (IFLoop[])), path) cf cm stm) (newPath (getBlock (rt, path)) path) path)
bodyFunctTranslator (Program (rt, path) cf cm stm@((IfElse expr body bodyelse):xs)) = bodyFunctTranslator (ifElseTranslator (Program ((changeTreeNewFunct rt path (IFLoop[])), path) cf cm stm) (newPath (getBlock (rt, path)) path) path)
bodyFunctTranslator (Program (rt, path) cf cm ((FunctionCall nm expr):xs)) = bodyFunctTranslator (Program ((callFunctionStmTranslator rt path (getFunct (rt, path) nm) expr cm nm), path) cf cm xs)

--Function for translating of call function as statement
callFunctionStmTranslator :: Block -> [Int] -> (Maybe Block) -> (Maybe [Expression]) -> ConstMap -> Name -> Block
callFunctionStmTranslator _ _ Nothing _ _ fname = error ("Function " ++ fname ++ "does not exist!")
callFunctionStmTranslator _ _ (Just (Funct _ _ _ (Just _) _ _ _ _)) Nothing _ fname = error ("Attempt to call function " ++ fname ++ ", with no args")
callFunctionStmTranslator _ _ (Just (Funct _ _ _ Nothing _ _ _ _)) (Just _) _ fname = error ("Attempt to call function " ++ fname ++ ", which has no args with some argument")
callFunctionStmTranslator blc path (Just (Funct _ fid "void" Nothing _ _ _ _)) Nothing _ _ = changeTreeNewBC blc path [CALL fid]
callFunctionStmTranslator blc path (Just (Funct _ fid "void" (Just ftp) _ _ _ _)) (Just expr) cm fname = changeTreeNewBC (loadArgs (blc, path) ftp expr cm fname) path [CALL fid]
callFunctionStmTranslator blc path (Just (Funct _ fid _ Nothing _ _ _ _)) Nothing _ _ = changeTreeNewBC blc path [(CALL fid), POP]
callFunctionStmTranslator blc path (Just (Funct _ fid _ (Just ftp) _ _ _ _)) (Just expr) cm fname = changeTreeNewBC (loadArgs (blc, path) ftp expr cm fname) path [(CALL fid), POP]

--Translator for if-statement.
ifElseTranslator :: Program -> [Int] -> [Int] -> Program
ifElseTranslator (Program (blc, path) cf cm stm@((IfElse expr body bodyelse):xs)) newpath oldpath = case (getBlock (blc, path)) of
	Nothing -> error " "
	Just (IFLoop _) -> ifElseTranslator (Program (blc, init path) cf cm stm) newpath oldpath
	Just (Funct _ _ _ _ bcc vm _ _) -> calcIfJump (ifBodyTranslator (addStartWhileJump (expressionTranslator blc path cm expr) path) path newpath cf cm body bodyelse) path oldpath xs bodyelse vm cm

--Initiate calculations of jump size in if-statement.
calcIfJump :: ((Block, Int), Int) -> [Int] -> [Int] -> [Statement] -> (Maybe [Statement]) -> VarMap -> ConstMap -> Program
calcIfJump ((blc, cf), lengthprev) path oldpath stm bodyelse vm cm = addFinalIfJump (blc, cf) (sizeofBC (byteCode (fromMaybe (nonFunct) (getBlock (blc, path)))) lengthprev) path oldpath stm bodyelse vm cm

--Restore VarMap and path. If we have else-part - call elseTranslator.
addFinalIfJump :: (Block, Int) -> (Int, Int, Int) -> [Int] -> [Int] -> [Statement] -> (Maybe [Statement]) -> VarMap -> ConstMap -> Program
addFinalIfJump (blc, cf) (start, fin, size) path oldpath stm bodyelse vm cm = if (isNothing bodyelse)
	then finishIf
	else (elseTranslator path bodyelse finishIf)
	where finishIf = finishLoop ((changeTreeChangeBC blc path (start - 1) (JA size)), cf) path vm oldpath cm stm

--If if-statement has else-part - start if-translator for this part.
elseTranslator path bodyelse (Program (blc, oldpath) cf cm stm) = calcIfJump (ifBodyTranslator (changeTreeNewFunct blc oldpath (IFLoop[])) path (newPath (getBlock (blc, oldpath)) oldpath) cf cm (fromMaybe [] bodyelse) Nothing) path oldpath stm Nothing (getVm (getBlock (blc, oldpath))) cm
	where getVm (Just (Funct _ _ _ _ _ vm _ _)) = vm

--Translator for for-loops.
forLoopTranslator :: Program -> [Int] -> [Int] -> Program
forLoopTranslator (Program (blc, path) cf cm stm@((ForLoop name expr1 expr2 body):xs)) newpath oldpath = case (getBlock (blc, path)) of
	Nothing -> error " "
	Just (IFLoop _) -> forLoopTranslator (Program (blc, init path) cf cm stm) newpath oldpath
	Just (Funct _ _ _ _ bcc vm _ _) -> finishLoop (loopBodyTranslator (forLoopHeadTranslator (expressionTranslator blc path cm expr1) (getVarInfo (blc, path) name) path cm expr2) path newpath cf cm body FOR) path vm oldpath cm xs

--Translator for while-loops.
whileLoopTranslator :: Program -> [Int] -> [Int] -> Program
whileLoopTranslator (Program (blc, path) cf cm stm@((WhileLoop expr body):xs)) newpath oldpath = case (getBlock (blc, path)) of
	Nothing -> error " "
	Just (IFLoop _) -> whileLoopTranslator (Program (blc, init path) cf cm stm) newpath oldpath
	Just (Funct _ _ _ _ bcc vm _ _) -> finishLoop (loopBodyTranslator ((addStartWhileJump (expressionTranslator blc path cm expr) path), (length bcc), Nothing) path newpath cf cm body WHILE) path vm oldpath cm xs

--Translator of head expression for for-loop.
forLoopHeadTranslator :: (Block, Type) -> (Maybe (Name, Id, Type, Id)) -> [Int] -> ConstMap -> Expression -> (Block, Int, Maybe(Name, Id, Type, Id))
forLoopHeadTranslator (blc, tpexpr) (Just (nm, fid, tpvar, vid)) path cm expr = if tpexpr /= "int" || tpvar /= "int"
	then error ("Wrong type in arguments of for-loop")
	else ((addStartForJump (forLoopSecExprTranslator (loadVar (Just (nm, fid, tpvar, vid)) ((assignmentToVar (blc, tpexpr) path (Just (nm, fid, tpvar, vid))), path)) path cm expr) path), (length (byteCode (fromMaybe (nonFunct) (getBlock (blc, path))))), Just(nm, fid, tpvar, vid))

--Auxiliary function to translate the second expression in condition of for-loop.
forLoopSecExprTranslator (blc, tp) path cm expr = expressionTranslator blc path cm expr

--Calculate size of list with bytecode.
sizeofBC :: [ByteCodeCommand] -> Int -> (Int, Int, Int)
sizeofBC bcc start = (start, (length bcc), foldl (\acc x -> acc + (commandSize x)) 0 (drop start bcc))
	where commandSize x = case x of
		(DLOAD _) -> 9
		(ILOAD _) -> 9
		(SLOAD _) -> 3
		(LOADDVAR _) -> 3
		(LOADIVAR _) -> 3
		(LOADSVAR _) -> 3
		(STOREDVAR _) -> 3
		(STORESVAR _) -> 3
		(STOREIVAR _) -> 3
		(LOADCTXIVAR _ _) -> 5
		(LOADCTXDVAR _ _) -> 5
		(LOADCTXSVAR _ _) -> 5
		(STORECTXIVAR _ _) -> 5
		(STORECTXDVAR _ _) -> 5
		(STORECTXSVAR _ _) -> 5
		(JA _) -> 3
		(IFICMPNE _) -> 3
		(IFICMPE _) -> 3
		(IFICMPG _) -> 3
		(IFICMPGE _) -> 3
		(IFICMPL _) -> 3
		(IFICMPLE _) -> 3
		(CALL _) -> 3
		(CALLNATIVE _) -> 3
		_ -> 1

--Returns empty funct.
nonFunct :: Block
nonFunct = Funct "0" 0 "" Nothing [] Map.empty 0 []

--We need to restore path and VarMap.
finishLoop :: (Block, Int) -> [Int] -> VarMap -> [Int] -> ConstMap -> [Statement] -> Program
finishLoop (blc, cf) path vm oldpath cm stm = Program ((replaceVarMap blc path vm), oldpath) cf cm stm

--Initiate calculations of jump size.
calcJump :: Program -> (Int, Int, Int) -> [Int] -> LoopType -> Maybe (Name, Id, Type, Id) -> (Block, Int)
calcJump (Program (rt, newpath) cf _ _) (start, fin, size) path lptp var = addFinalJump (rt, path) cf (start, fin, size) (sizeofBC (byteCode (fromMaybe (nonFunct) (getBlock (rt, path)))) fin) lptp var

--Add some code to back to the condition.
addFinalJump :: SubBlock -> Int -> (Int, Int, Int) -> (Int, Int, Int) -> LoopType -> Maybe (Name, Id, Type, Id) -> (Block, Int)
addFinalJump (blc, path) cf (start1, fin1, size1) (start2, fin2, size2) lptp  var = case lptp of
	WHILE -> ((changeTreeChangeBC (changeTreeNewBC blc path [JA (-size1 - size2 - 3)]) path (fin1 - 1) (JA (size2 + 3))), cf)
	--FOR -> ((changeTreeChangeBC (changeTreeNewBC blc path [ILOAD1, IADD, (JA (-size1 - size2 - 5))]) path (fin1 - 1) (JA (size2 + 5))), cf)
	FOR -> ((changeTreeChangeBC (changeTreeNewBC (fst (loadVar var (blc, path))) path [ILOAD1, IADD, (JA (-size1 - size2 - (size3 var) - 5))]) path (fin1 - 1) (JA (size2 + (size3 var) + 5))), cf)
		where size3 (Just (nm, fid, vtp, vid)) = if fid /= (getCurrentFid (blc, path))
			then 5
			else if vid < 4
				then 1
				else 3 

--Translator of if-body. If we have else-part - add JA-command to jump over it.
ifBodyTranslator :: Block -> [Int] -> [Int] -> Int -> ConstMap -> [Statement] -> (Maybe [Statement]) -> ((Block, Int), Int)
ifBodyTranslator blc path newpath cf cm body bodyelse = (maybeElse (bodyFunctTranslator (Program (blc, newpath) cf cm body)), (length (byteCode (fromMaybe (nonFunct) (getBlock (blc, path))))))
	where maybeElse (Program (blc, _) cf _ _) = case bodyelse of
		Nothing -> (blc, cf)
		Just stm -> ((changeTreeNewBC blc path [JA 0]), cf)
	
--Translate the body of loop.
loopBodyTranslator :: (Block, Int, Maybe(Name, Id, Type, Id)) -> [Int] -> [Int] -> Int -> ConstMap -> [Statement] -> LoopType -> (Block, Int)
loopBodyTranslator (blc, lengprev, var) path newpath cf cm body lptp = calcJump (bodyFunctTranslator (Program (blc, newpath) cf cm body)) (sizeofBC (byteCode (fromMaybe (nonFunct) (getBlock (blc, path)))) lengprev) path lptp var

--Add some code to check the condition of entering to the for loop.
addStartForJump :: (Block, Type) -> [Int] -> Block
addStartForJump (blc, tpexpr) path = case tpexpr of
	"int" -> changeTreeNewBC blc path [IFICMPL 5, POP, POP, JA 5, POP, POP, JA 0]
	_ -> error " "

--Add some code to check the condition of entering to the while loop.
addStartWhileJump :: (Block, Type) -> [Int] -> Block
addStartWhileJump (blc, tpexpr) path = case tpexpr of
	"string" -> changeTreeNewBC blc path [POP, JA 0]
	"int" -> changeTreeNewBC blc path [ILOAD0, IFICMPE 5, POP, POP, JA 5, POP, POP, JA 0]
	"double" -> changeTreeNewBC blc path [DLOAD0, DCMP, SWAP, POP, SWAP, POP, ILOAD0, IFICMPE 5, POP, POP, JA 5, POP, POP, JA 0]

--Translator for "print" statement. 
--N.B. If print hasn't args - add DUMP command to bytecode.
printStTranslator :: SubBlock -> (Maybe [Expression]) -> ConstMap -> Block
printStTranslator (blc, path) Nothing _ = changeTreeNewBC blc path [DUMP]
printStTranslator (blc, _) (Just []) _ = blc
printStTranslator (blc, path) (Just (ex:pr)) cm = printStTranslator (printExprTranslator (expressionTranslator blc path cm ex) path, path) (Just pr) cm

--Check type of print_expression and print it.
printExprTranslator :: (Block, Type) -> [Int] -> Block
printExprTranslator (blc, exprtp) path = case exprtp of
	"int" -> changeTreeNewBC blc path [IPRINT, POP]
	"double" -> changeTreeNewBC blc path [DPRINT, POP]
	"string" -> changeTreeNewBC blc path [SPRINT, POP]
	_ -> error ("Unexpected error")

--Translator for "return" statement
returnStTranslator :: SubBlock -> (Maybe Expression) -> ConstMap -> Block
returnStTranslator (blc, path) jexpr cm = case (getBlock (blc, path)) of
	Just (IFLoop _) -> returnStTranslator (blc, init path) jexpr cm
	Just (Funct nm _ rtp _ _ _ _ _) -> case jexpr of
		Nothing -> case (rtp) of
			("void") -> case path of
				[] -> changeTreeNewBC blc [] [STOP]
				_ -> changeTreeNewBC blc path [RETURN]
			(_) -> error ("Non-void function " ++ nm ++ " try to return void value instead of " ++ rtp)
		(Just expr) -> changeTreeNewBC (checkTypeOfArg (expressionTranslator blc path cm expr) path rtp "return") path [RETURN]

--Check if function returns "void" value, then add in the end of its bytecode command RETURN. Else - do nothing.
isVoidFunct :: SubBlock -> Block
isVoidFunct (blc, path) = case (getBlock (blc, path)) of
	Nothing -> error ("Unexpected error")
	Just (IFLoop _) -> blc
	Just (Funct _ _ rtp _ _ _ _ _) -> if rtp == "void"
		then changeTreeNewBC blc path [RETURN]
		else blc

--Translator for function. Create new Funct and start bodyFunctTranslator.
functionHeadTranslator :: Program -> [Int] -> Program
functionHeadTranslator (Program (blc, path) cf cm ((Function rtp nm Nothing (Right body)):xs)) newpath = retF (bodyFunctTranslator (Program ((addFunction (blc, path) nm (Funct nm cf rtp Nothing [] Map.empty 0 [])), newpath) (cf + 1) cm body)) path xs
functionHeadTranslator (Program (blc, path) cf cm ((Function rtp nm (Just tp) (Right body)):xs)) newpath = retF (bodyFunctTranslator (Program ((loadVarArgs ((addFunction (blc, path) nm (Funct nm cf rtp (loadTypes tp (Just []) ) [] Map.empty 0 [])), newpath) tp), newpath) (cf + 1) cm body)) path xs

--Auxiliary function. Restore old path after translating of subfunction.
retF:: Program -> [Int] -> [Statement] -> Program
retF (Program (blc, opath) cf cm oast) npath nast = Program (blc, npath) cf cm nast

--Auxiliary function. Returns path to new subfunction.
newPath :: Maybe Block -> [Int] -> [Int]
newPath Nothing _ = error ("Unexpected error")
newPath (Just blc) path = path ++ [length (bl blc)]

--Adding new function.
addFunction :: SubBlock -> Name -> Block -> Block
addFunction (blc, path) nm func = case (getFunct (blc, path) nm) of
	Nothing -> changeTreeNewFunct blc path func
	Just _ -> error ("Function with name " ++ nm ++ " is already defined in this context")

--Auxiliary function. Returns list of arguments type.
loadTypes :: [Statement] -> (Maybe [Type]) -> (Maybe [Type])
loadTypes [] (Just []) = Nothing
loadTypes [] tplist = tplist 
loadTypes ((Declaration tp _) :declor) tplist = loadTypes declor ((++[tp]) <$> tplist)

--Auxiliary function. Create arg variables in new function.
loadVarArgs :: SubBlock -> [Statement] -> Block
loadVarArgs (blc, path) ((Declaration tp nm):declor) = loadVarArgs ((declarationTranslator tp nm blc path), path) declor
loadVarArgs (blc, path) _ = blc

--Function for searching variable in current function. Returns information about variable.
findVar :: SubBlock -> Name -> Maybe (Int, Type)
findVar (blc, path) nm = case (getBlock (blc, path)) of
	Just (IFLoop _) -> findVar (blc, (init path)) nm
	Just (Funct _ _ _ _ _ vm _ _ ) -> Map.lookup nm vm
	--Nothing -> error "Unexpected error"

--Function for adding variable to current functions map.
addVar :: SubBlock -> Name -> Type -> Block
addVar (blc, path) nm tp = case (getBlock (blc, path)) of
	Just (IFLoop _) -> addVar (blc, (init path)) nm tp
	Just (Funct _ _ _ _ _ _ _ _) -> changeTreeNewVar blc path nm tp
	--Nothing -> error "Unexpected error"

--Function for searching variable in current function or context of its parents. Returns information about variable and function id.
getVarInfo :: SubBlock -> Name -> Maybe (Name, Id, Type, Id)
getVarInfo (blc, []) nm = case (Map.lookup nm (varMap blc)) of
	Nothing -> error ("Attempt to use variable " ++ nm ++ ", which does not exist")
	Just (vid, vtp) -> return (nm, (getCurrentFid (blc, [])), vtp, vid)
getVarInfo (blc, path) nm = case (getBlock (blc, path)) of
	Just (IFLoop _) -> getVarInfo (blc, init path) nm
	Just (Funct fnm fid _ _ _ vm _ _) -> case (Map.lookup nm vm) of
		Nothing -> getVarInfo (blc, init path) nm
		Just (vid, vtp) -> return (nm, fid, vtp, vid)

--Get Id of current function.
getCurrentFid :: SubBlock -> Id
getCurrentFid (blc, path) = case (getBlock (blc, path)) of
	Nothing -> error "Unexpected error"
	Just (IFLoop _) -> getCurrentFid (blc, init path)
	Just (Funct _ fid _ _ _ _ _ _) -> fid

--Get Id of string in constant map.
getStringId :: String -> ConstMap -> Id
getStringId s cm = case (Map.lookup s cm) of
	Nothing -> error ("Can't find this string. How it's possible?")
	Just x -> x

--Auxiliary function. It is used as find predicate in getFunct.
isNeededFunct :: Name -> Block -> Bool
isNeededFunct nm (IFLoop _) = False
isNeededFunct nm func = ((name func) == nm)

--Get function by its name. Function must be a child of current function or existing in its parents context.
getFunct :: SubBlock -> Name -> Maybe Block
getFunct (blc, []) nm = find (isNeededFunct nm) (bl blc)
getFunct (blc, path) nm = case (getBlock (blc, path)) of
	Just block -> case (find (isNeededFunct nm) (bl block)) of
		Nothing -> getFunct (blc, init path) nm
		Just func -> return func
	Nothing -> error ("Unexpected error")

--Auxiliary function. When we load args of function - we need to check type of args. For function loadArgs.
checkTypeOfArg :: (Block, Type) -> [Int] -> Type -> Name -> Block
checkTypeOfArg (blc, acttp) path ftp nm = if (acttp == ftp)
	then blc
	else case (acttp, ftp) of
		("int", "double") -> changeTreeNewBC blc path [I2D]
		("double", "int") -> changeTreeNewBC blc path [D2I]
		("string", "int") -> changeTreeNewBC blc path [S2I]
		("string", "double") -> changeTreeNewBC blc path [S2I, I2D]
		(_, _) -> case nm of
			"return" -> error ("Attempt to return value with type " ++ acttp ++ " from function with type " ++ ftp)
			_ -> error ("Attempt to call function " ++ nm ++ " with argument of type " ++ acttp ++ " instead of type " ++ ftp)

--Loading arguments, when we call some function.
loadArgs :: SubBlock -> [Type] -> [Expression] -> ConstMap -> Name -> Block
loadArgs (blc, path) [] [] _ nm = blc
loadArgs _ [] expr _ nm = error ("Too many arguments in call of function " ++ nm)
loadArgs _ argtp [] _ nm = error ("Too few arguments in call of function " ++ nm)
loadArgs (blc, path) (a:rgtp) (e:xpr) cm nm = loadArgs ((checkTypeOfArg (expressionTranslator blc path cm e) path a nm), path) rgtp xpr cm nm

--Function for translating of call function as expression
callFunctionExprTranslator :: (Maybe Block) -> (Maybe [Expression]) -> Block -> [Int] -> ConstMap -> Name -> (Block, Type)
callFunctionExprTranslator Nothing _ _ _ _ fname = error ("Function " ++ fname ++ " does not exist!")
callFunctionExprTranslator (Just (Funct _ _ _ (Just _) _ _ _ _)) Nothing _ _ _ fname = error ("Attempt to call function " ++ fname ++ " with no args")
callFunctionExprTranslator (Just (Funct _ _ _ Nothing _ _ _ _)) (Just _) _ _ _ fname = error ("Attemt to call function " ++ fname ++ ", which has no args with some arguments")
callFunctionExprTranslator (Just (Funct _ _ "void" _ _ _ _ _)) _ _ _ _ fname = error ("Attempt to call returning void function " ++ fname ++ " as part of expression")
callFunctionExprTranslator (Just (Funct _ fid fret Nothing _ _ _ _)) Nothing blc path _ _ = ((changeTreeNewBC blc path [(CALL fid)]), fret)
callFunctionExprTranslator (Just (Funct _ fid fret (Just ftp) _ _ _ _)) (Just expr) blc path cm fname = ((changeTreeNewBC (loadArgs (blc, path) ftp expr cm fname) path [(CALL fid)]), fret)

--Function for adding byte code commands of loading value from variable to stack. For assignments.
loadVar :: Maybe (Name, Id, Type, Id) -> SubBlock -> (Block, Type)
loadVar (Just (nm, fid, vtp, vid)) (blc, path) = case (getBlock (blc, path)) of
	Just (IFLoop _) -> loadVar (Just (nm, fid, vtp, vid)) (blc, init path)
	Just (Funct _ cfid _ _ _ _ _ _) -> if cfid == fid
		then case vtp of
			"double" -> case vid of
				0 -> ((changeTreeNewBC blc path [(LOADDVAR0)]), "double")
				1 -> ((changeTreeNewBC blc path [(LOADDVAR1)]), "double")
				2 -> ((changeTreeNewBC blc path [(LOADDVAR2)]), "double")
				3 -> ((changeTreeNewBC blc path [(LOADDVAR3)]), "double")
				_ -> ((changeTreeNewBC blc path [(LOADDVAR vid)]), "double")
			"int" -> case vid of
				0 -> ((changeTreeNewBC blc path [(LOADIVAR0)]), "int")	
				1 -> ((changeTreeNewBC blc path [(LOADIVAR1)]), "int")
				2 -> ((changeTreeNewBC blc path [(LOADIVAR2)]), "int")
				3 -> ((changeTreeNewBC blc path [(LOADIVAR3)]), "int")
				_ -> ((changeTreeNewBC blc path [(LOADIVAR vid)]), "int")
			"string" -> case vid of
				0 -> ((changeTreeNewBC blc path [(LOADSVAR0)]), "string")
				1 -> ((changeTreeNewBC blc path [(LOADSVAR1)]), "string")
				2 -> ((changeTreeNewBC blc path [(LOADSVAR2)]), "string")
				3 -> ((changeTreeNewBC blc path [(LOADSVAR3)]), "string")
				_ -> ((changeTreeNewBC blc path [(LOADSVAR vid)]), "string")
		else case vtp of
			"double" -> ((changeTreeNewBC blc path [(LOADCTXDVAR fid vid)]), "double")
			"int" -> ((changeTreeNewBC blc path [(LOADCTXIVAR fid vid)]), "int")
			"string" -> ((changeTreeNewBC blc path [(LOADCTXSVAR fid vid)]), "string")

--Function for adding byte code commands of loading value from stack into variable. For assignments.
assignmentToVar :: (Block, Type) -> [Int] -> Maybe (Name, Id, Type, Id) -> Block
assignmentToVar (blc, tps) path (Just (nm, fid, vtp, vid)) = case (getBlock (blc, path)) of
	--Nothing -> error "Unexpected error"
	Just (IFLoop _) -> assignmentToVar (blc, tps) (init path) (Just (nm, fid, vtp, vid))
	Just (Funct _ cfid _ _ bcc _ _ _) -> if cfid == fid
		then if tps == vtp
			then case tps of
				"double" ->  case vid of
					0 -> changeTreeNewBC blc path [(STOREDVAR0)]
					1 -> changeTreeNewBC blc path [(STOREDVAR1)]
					2 -> changeTreeNewBC blc path [(STOREDVAR2)]
					3 -> changeTreeNewBC blc path [(STOREDVAR3)]
					_ -> changeTreeNewBC blc path [(STOREDVAR vid)]
				"int" -> case vid of
					0 -> changeTreeNewBC blc path [(STOREIVAR0)]
					1 -> changeTreeNewBC blc path [(STOREIVAR1)]
					2 -> changeTreeNewBC blc path [(STOREIVAR2)]
					3 -> changeTreeNewBC blc path [(STOREIVAR3)]
					_ -> changeTreeNewBC blc path [(STOREIVAR vid)]
				"string" -> case vid of
					0 -> changeTreeNewBC blc path [(STORESVAR0)]
					1 -> changeTreeNewBC blc path [(STORESVAR1)]
					2 -> changeTreeNewBC blc path [(STORESVAR2)]
					3 -> changeTreeNewBC blc path [(STORESVAR3)]
					_ -> changeTreeNewBC blc path [(STORESVAR vid)]
			else case (vtp, tps) of
				("double", "int") -> case vid of
					0 -> changeTreeNewBC blc path [(I2D), (STOREDVAR0)]
					1 -> changeTreeNewBC blc path [(I2D), (STOREDVAR1)]
					2 -> changeTreeNewBC blc path [(I2D), (STOREDVAR2)]
					3 -> changeTreeNewBC blc path [(I2D), (STOREDVAR3)]
					_ -> changeTreeNewBC blc path [(I2D), (STOREDVAR vid)]
				("int", "double") -> case vid of
					0 -> changeTreeNewBC blc path [(D2I), (STOREIVAR0)]
					1 -> changeTreeNewBC blc path [(D2I), (STOREIVAR1)]
					2 -> changeTreeNewBC blc path [(D2I), (STOREIVAR2)]
					3 -> changeTreeNewBC blc path [(D2I), (STOREIVAR3)]
					_ -> changeTreeNewBC blc path [(D2I), (STOREIVAR vid)]
				("double", "string") -> case vid of
					0 -> changeTreeNewBC blc path [(S2I), (I2D), (STOREDVAR0)]
					1 -> changeTreeNewBC blc path [(S2I), (I2D), (STOREDVAR1)]
					2 -> changeTreeNewBC blc path [(S2I), (I2D), (STOREDVAR2)]
					3 -> changeTreeNewBC blc path [(S2I), (I2D), (STOREDVAR3)]
					_ -> changeTreeNewBC blc path [(S2I), (I2D), (STOREDVAR vid)]
				("int", "string") -> case vid of
					0 -> changeTreeNewBC blc path [(S2I), (STOREIVAR0)]
					1 -> changeTreeNewBC blc path [(S2I), (STOREIVAR1)]
					2 -> changeTreeNewBC blc path [(S2I), (STOREIVAR2)]
					3 -> changeTreeNewBC blc path [(S2I), (STOREIVAR3)]
					_ -> changeTreeNewBC blc path [(S2I), (STOREIVAR3)]
				(_,_) -> error ("Attempt to cast type " ++ tps ++ " to type " ++ vtp ++ " in assignment to variable " ++ nm)
		else if tps == vtp
			then case tps of 
				"double" -> changeTreeNewBC blc path [(STORECTXDVAR fid vid)]
				"int" -> changeTreeNewBC blc path [(STORECTXIVAR fid vid)]
				"string" -> changeTreeNewBC blc path [(STORECTXSVAR fid vid)]
			else case (vtp, tps) of
				("double", "int") -> changeTreeNewBC blc path [(I2D), (STORECTXDVAR fid vid)]
				("int", "double") -> changeTreeNewBC blc path [(D2I), (STORECTXIVAR fid vid)]
				("double", "string") -> changeTreeNewBC blc path [(S2I), (I2D), (STORECTXDVAR fid vid)]
				("int", "string") -> changeTreeNewBC blc path [(S2I), (STORECTXIVAR fid vid)]
				(_,_) -> error ("Attempt to cast type " ++ tps ++ " to type " ++ vtp ++ " in assignment to variable " ++ nm)

--Byte code for not operation.
notOperation :: Type -> [ByteCodeCommand]
notOperation "int" = [(ILOAD0), (IFICMPNE 6), (POP), (POP), (ILOAD1), (JA 2), (SWAP), (POP)]
--Are we really need this?..
notOperation "double" = [(DLOAD0), (DCMP), (SWAP), (POP), (SWAP), (POP), (ILOAD0), (IFICMPNE 6), (POP), (POP), (DLOAD1), (JA 3), (POP), (POP), (DLOAD0)]

--Function for adding byte code commands of unary operation.
unarOperation :: Unar -> (Block, Type) -> [Int] -> (Block, Type)
unarOperation _ (_, "string") _ = error ("Attempt to use unary operation with string value")
unarOperation Not (blc, tp) path = ((changeTreeNewBC blc path (notOperation tp)), tp)
unarOperation Neg (blc, "int") path = ((changeTreeNewBC blc path [(INEG)]), "int")
unarOperation Neg (blc, "double") path = ((changeTreeNewBC blc path [(DNEG)]), "double")

--Calculation of binary operations arguments.
binarOperationPrev :: Binar -> ConstMap -> Expression -> (Block, Type) -> [Int] -> (Block, Type)
binarOperationPrev bin cm subexpr1 (blc, tp) path = binaryOperation bin (expressionTranslator blc path cm subexpr1) tp path

--Adding byte code commands of binary operation.
binaryOperation :: Binar -> (Block, Type) -> Type -> [Int] -> (Block, Type)
binaryOperation bin (blc, tp1) tp2 path = ((changeTreeNewBC blc path (binarBC bin tp1 tp2)), (binarBCType bin tp1 tp2))

--Auxiliary function. Returns byte code of specific binary operation. Typecast part.
binarBC :: Binar -> Type -> Type -> [ByteCodeCommand]
binarBC _ "string" _ = error ("Attempt to use binary operator with string value")
binarBC _ _ "string" = error ("Attempt to use binary operator with string value")
binarBC Mod tp1 tp2 = if (tp1 /= "int" || tp2 /= "int")
	then error ("Attempt to use Mod operation with double variable")
	else [IMOD]
binarBC bin tp1 tp2 = case (tp1, tp2) of
	--("int", "int") -> ((changeTreeNewBC blc path (binarOpBC "int" bin)), "int")
	("int", "int") -> binarOpBC "int" bin
	("int", "double") -> ([(I2D)] ++ (binarOpBC "double" bin))
	("double", "int") -> ([(SWAP), (I2D), (SWAP)] ++ (binarOpBC "double" bin))
	("double", "double") -> binarOpBC "double" bin

--Auxiliary function. Returns byte code of specific binary operation. Operation part.
binarOpBC :: Type -> Binar -> [ByteCodeCommand]
binarOpBC "int" Sum = [IADD]
binarOpBC "double" Sum = [DADD] 
binarOpBC "int" Sub = [ISUB]
binarOpBC "double" Sub = [DSUB]
binarOpBC "int" Mult = [IMUL]
binarOpBC "double" Mult = [DMUL]
binarOpBC "int" Div = [IDIV]
binarOpBC "double" Div = [DDIV]
binarOpBC "int" Eq = [(IFICMPE 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Eq = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOAD0), (IFICMPNE 6), (POP), (POP), (ILOAD1), (JA 2), (SWAP), (POP)]
binarOpBC "int" Neq = [(IFICMPNE 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Neq = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOAD0), (IFICMPE 6), (POP), (POP), (ILOAD1), (JA 1), (POP)]
binarOpBC "int" Gt = [(IFICMPG 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Gt = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOAD1), (IFICMPE 6), (POP), (POP), (ILOAD0), (JA 1), (POP)]
binarOpBC "int" Lw = [(IFICMPL 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Lw = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOADM1), (IFICMPE 6), (POP), (POP), (ILOAD0), (JA 3), (POP), (POP), (ILOAD1)]
binarOpBC "int" Gte = [(IFICMPGE 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Gte = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOADM1), (IFICMPE 6), (POP), (POP), (ILOAD1), (JA 3), (POP), (POP), (ILOAD0)]
binarOpBC "int" Lwe = [(IFICMPLE 4), (ILOAD0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
binarOpBC "double" Lwe = [(DCMP), (SWAP), (POP), (SWAP), (POP), (ILOAD1), (IFICMPE 6), (POP), (POP), (ILOAD0), (JA 2), (SWAP), (POP)]
binarOpBC "int" And = [IAAND]
binarOpBC "double" And = [(DLOAD0), (DCMP), (ILOAD0), (IFICMPE 23), (POP), (POP), (SWAP), (POP), (DCMP), (ILOAD0), (IFICMPE 9), (POP), (POP), (POP), (POP), (ILOAD1), (JA 6), (POP), (POP), (POP), (POP), (POP), (ILOAD0)]
binarOpBC "int" Or = [IAOR]
binarOpBC "double" Or = [(DLOAD0), (DCMP), (ILOAD0), (IFICMPNE 23), (POP), (POP), (SWAP), (POP), (DCMP), (ILOAD0), (IFICMPNE 9), (POP), (POP), (POP), (POP), (ILOAD1), (JA 6), (POP), (POP), (POP), (POP), (POP), (ILOAD0)]

--Auxiliary function. Returns resulting type of binary operation.
binarBCType :: Binar -> Type -> Type -> Type
binarBCType _ "int" "int" = "int"
binarBCType Mod _ _ = error ("")
binarBCType bin _ _ = if (bin == Sum || bin == Sub || bin == Mult || bin == Div)
	then "double"
	else "int"

--If path points to the IFLoop block, we must move up before add a new var.
changeTreeNewVar blc path nm tp = case (getBlock (blc, path)) of
	Just (IFLoop _) -> changeTreeNewVar blc (init path) nm tp
	_ -> changeTreeNewVarF blc path nm tp

--Adding variable to function map. Returns new tree.
changeTreeNewVarF :: Block -> [Int] -> Name -> Type -> Block
changeTreeNewVarF blc [] nm tp = Funct (name blc) (idf blc) (retType blc) (tP blc) (byteCode blc) (Map.insert nm ((Map.size (varMap blc)), tp) (varMap blc)) (max (maxVar blc) ((Map.size (varMap blc)) + 1)) (bl blc)
changeTreeNewVarF blc (p:ath) nm tp = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewVarF (blc2 !! p) ath nm tp) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewVarF (blc3 !! p) ath nm tp) blc3) 

--If path points to the IFLoop block, we must move up before add new bytecode commands.
changeTreeNewBC blc path bcc = case (getBlock (blc, path)) of
	Just (IFLoop _) -> changeTreeNewBC blc (init path) bcc
	_ -> changeTreeNewBCF blc path bcc

--Adding bytecode to function. Returns new tree.
changeTreeNewBCF :: Block -> [Int] -> [ByteCodeCommand] -> Block
changeTreeNewBCF blc [] bcc = Funct (name blc) (idf blc) (retType blc) (tP blc) ((byteCode blc) ++ bcc) (varMap blc) (maxVar blc) (bl blc)
changeTreeNewBCF blc (p:ath) bcc = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewBCF (blc2 !! p) ath bcc) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewBCF (blc3 !! p) ath bcc) blc3)

--Adding function to current function subblocks. Returns new tree.
changeTreeNewFunct :: Block -> [Int] -> Block -> Block
changeTreeNewFunct rt [] blc = case rt of
	(IFLoop blc2) -> (IFLoop (blc2 ++ [blc]))
	_ -> Funct (name rt) (idf rt) (retType rt) (tP rt) (byteCode rt) (varMap rt) (maxVar rt) ((bl rt) ++ [blc])
changeTreeNewFunct rt (p:ath) blc = case rt of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewFunct (blc2 !! p) ath blc) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewFunct (blc3 !! p) ath blc) blc3)

--Replace command "place" in list of bytecode commands with command bcc.
changeTreeChangeBC :: Block -> [Int] -> Int -> ByteCodeCommand -> Block
changeTreeChangeBC blc [] place bcc = Funct (name blc) (idf blc) (retType blc) (tP blc) (set (element place) bcc (byteCode blc)) (varMap blc) (maxVar blc) (bl blc)
changeTreeChangeBC blc (p:ath) place bcc = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeChangeBC (blc2 !! p) ath place bcc) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeChangeBC (blc3 !! p) ath place bcc) blc3)

--Restore VarMap after translating IFLoop block.
replaceVarMap :: Block -> [Int] -> VarMap -> Block
replaceVarMap blc [] vmr = Funct (name blc) (idf blc) (retType blc) (tP blc) (byteCode blc) vmr (maxVar blc) (bl blc)
replaceVarMap blc (p:ath) vmr = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (replaceVarMap (blc2 !! p) ath vmr) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (replaceVarMap (blc3 !! p) ath vmr) blc3)

--Translator for declarations.
declarationTranslator :: Type -> Name -> Block -> [Int] -> Block
declarationTranslator tp nm blc path = case (findVar (blc, path) nm) of
	Nothing -> addVar (blc, path) nm tp
	Just _ -> error ("Can't declare variable " ++ show nm ++ " . It's already exist in this function!")

--Translator for assignments.
assignmentTranslator :: Block -> [Int] -> Int -> ConstMap -> Name -> Expression -> Block
assignmentTranslator blc path cf cm nm expr = assignmentToVar (expressionTranslator blc path cm expr) path (getVarInfo (blc, path) nm)

--Translator for expressions.
expressionTranslator :: Block -> [Int] -> ConstMap -> Expression -> (Block, Type)
expressionTranslator blc path cm expr = case expr of
	Ident nm -> (loadVar (getVarInfo (blc,path) nm) (blc, path))
	Str s -> ((changeTreeNewBC blc path [(SLOAD (getStringId s cm))]), "string")
	IntNumb inumb -> ((changeTreeNewBC blc path [(ILOAD (fromIntegral(inumb)))]), "int")
	FloatNumb fnumb -> ((changeTreeNewBC blc path [(DLOAD fnumb)]), "double")
	FCall fname args -> callFunctionExprTranslator (getFunct (blc, path) fname) args blc path cm fname
	UnarExpr un subexpr -> unarOperation un (expressionTranslator blc path cm subexpr) path
	BinaryExpr bin subexpr1 subexpr2 -> binarOperationPrev bin cm subexpr1 (expressionTranslator blc path cm subexpr2) path
