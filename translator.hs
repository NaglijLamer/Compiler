
module Translator
(
	--mainTranslator
	bodyFunctTranslator
) where

import Parser
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Data.List
import Control.Lens

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
			| CALL Int | CALLNATIVE Int | RETURN | BREAK deriving Show

--mainTranslator :: [Statement] -> (Either Error ([ByteCodeCommand], Header)
--mainTranslator = 
--Test main function.
main =
	do { c <- getContents
		;case (parse bodyParser "stdin") c of
			Left e -> do {
				putStrLn "There is a syntax error in your program!"
			;	print e }
			Right r -> do {
				mapM_ print r
			;	mapM_ print (Map.toList((varMap (fst(root (rootInitFunction r)))))) 
			;	mapM_ print (byteCode(fst(root(rootInitFunction r)))) }}

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
-- I'm not sure that this branch must work so. Test version, cause empty st means the end of function/block. But this is noce
bodyFunctTranslator (Program (rt, path) cf cm []) = Program (rt, path) cf cm []
bodyFunctTranslator (Program (rt, path) cf cm ((Declaration tp nm):xs)) = bodyFunctTranslator (Program ((declarationTranslator tp nm rt path), path) cf cm xs)
bodyFunctTranslator (Program (rt, path) cf cm ((Assignment nm expr):xs)) = bodyFunctTranslator (Program ((assignmentTranslator rt path cf cm nm expr), path) cf cm xs)

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
getVatInfo (blc, []) nm = case (Map.lookup nm (varMap blc)) of
	Nothing -> error ("Attrmpt to use variable " ++ nm ++ " , which does not exist")
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

--Get function by its id. Function must be a child of current function or existing in its parents context.
getFunct :: SubBlock -> Name -> Maybe Block
getFunct (blc, []) nm = find (isNeededFunct nm) (bl blc)
getFunct (blc, path) nm = case (getBlock (blc, path)) of
	Just block -> case (find (isNeededFunct nm) (bl block)) of
		Nothing -> getFunct (blc, init path) nm
		Just func -> return func
	Nothing -> error ("Unexpected error")

--Auxiliary function. When we load args of function - we need to check type of args. For function loadArgs.
checkTypeOfArg :: (Block, Type) -> Type -> Name -> Block
checkTypeOfArg (blc, acttp) ftp nm = if (acttp == ftp)
	then blc
	else error ("Attempt to call function " ++ nm ++ " with argument of type " ++ acttp ++ " instead of type " ++ ftp)

--Loading arguments, when we call some function.
loadArgs :: SubBlock -> [Type] -> [Expression] -> ConstMap -> Name -> Block
loadArgs (blc, path) (a:rgtp) (e:xpr) cm nm = loadArgs ((checkTypeOfArg (expressionTranslator blc path cm e) a nm), path) rgtp xpr cm nm

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
binarOpBC "int" Eq = [(IFICMPE 4), (ILOAD 0), (JA 1), (ILOAD1), (SWAP), (POP), (SWAP), (POP)]
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

--Adding variable to function map. Returns new tree.
changeTreeNewVar :: Block -> [Int] -> Name -> Type -> Block
changeTreeNewVar blc [] nm tp = Funct (name blc) (idf blc) (retType blc) (tP blc) (byteCode blc) (Map.insert nm ((Map.size (varMap blc)), tp) (varMap blc)) ((maxVar blc)+1) (bl blc)
changeTreeNewVar blc (p:ath) nm tp = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewVar (blc2 !! p) ath nm tp) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewVar (blc3 !! p) ath nm tp) blc3) 

--Adding bytecode to function. Returns new tree.
changeTreeNewBC :: Block -> [Int] -> [ByteCodeCommand] -> Block
changeTreeNewBC blc [] bcc = Funct (name blc) (idf blc) (retType blc) (tP blc) ((byteCode blc) ++ bcc) (varMap blc) (maxVar blc) (bl blc)
changeTreeNewBC blc (p:ath) bcc = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewBC (blc2 !! p) ath bcc) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewBC (blc3 !! p) ath bcc) blc3)

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
