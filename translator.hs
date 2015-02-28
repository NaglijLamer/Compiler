
module Translator
(
	--mainTranslator
	bodyFunctTranslator
) where

import Parser
import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Control.Lens

data ByteCodeCommand = DLOAD Double | ILOAD Integer | SLOAD Integer
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

-- type Type = String
type RetType = String
type Id = Int

data Block = Funct { name :: String
			, idf :: Id
			, retType :: RetType
			, tP :: (Maybe [Type])
			, byteCode :: [ByteCodeCommand]
			, varMap :: VarMap
			, maxVar :: Int 
			, bl :: [Block] }
		| IFLoop { bl :: [Block] } deriving Show
type SubBlock = (Block, [Int])

getBlock :: SubBlock -> Maybe (Block)
getBlock (tree, []) = Just tree
getBlock (tree, path) = getBlock ((bl tree) !! (head path), (tail path))
--getBlock' :: Block -> [Int] -> (Maybe Block)
--getBlock' tree [] = Just tree
--getBlock' tree path = getBlock ((bl tree) !! (head path) (tail path))

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

type VarMap = Map.Map String (Id, Type)

type ConstMap = Map.Map String Id

data Program = Program { root :: SubBlock
			, countF :: Int
			, constantMap :: ConstMap
			, ast :: [Statement] } deriving Show

rootInitFunction :: [Statement] -> Program
rootInitFunction st = bodyFunctTranslator (Program ((Funct "0root" 0 "void" Nothing [] Map.empty 0 []), []) 1 Map.empty st)

bodyFunctTranslator :: Program -> Program
-- I'm not sure that this branch must work so. Test version, cause empty st means the end of function/block. But this is noce
bodyFunctTranslator (Program (rt, path) cf cm []) = Program (rt, path) cf cm []
bodyFunctTranslator (Program (rt, path) cf cm ((Declaration tp nm):xs)) = bodyFunctTranslator (Program ((declarationTranslator tp nm rt path), path) cf cm xs)
bodyFunctTranslator (Program (rt, path) cf cm ((Assignment nm expr):xs)) = bodyFunctTranslator (Program ((assignmentTranslator rt path cf cm nm expr), path) cf cm xs)

findVar :: SubBlock -> Name -> Maybe (Int, Type)
findVar (blc, path) nm = case (getBlock (blc, path)) of
	Just (IFLoop _) -> findVar (blc, (init path)) nm
	Just (Funct _ _ _ _ _ vm _ _ ) -> Map.lookup nm vm
	Nothing -> error "Unexpected error"

addVar :: SubBlock -> Name -> Type -> Block
addVar (blc, path) nm tp = case (getBlock (blc, path)) of
	Just (IFLoop _) -> addVar (blc, (init path)) nm tp
	Just (Funct _ _ _ _ _ _ _ _) -> changeTreeNewVar blc path nm tp
	Nothing -> error "Unexpected error"

getVarInfo :: SubBlock -> Name -> Maybe (Name, Id, Type, Id)
getVarInfo (blc, path) nm = case (getBlock (blc, path)) of
	Nothing -> error ("Attempt to use variable " ++ nm ++ ", which does not exist")
	Just (IFLoop _) -> getVarInfo (blc, init path) nm
	Just (Funct _ fid _ _ _ vm _ _) -> case (Map.lookup nm vm) of
		Nothing -> getVarInfo (blc, init path) nm
		Just (vid, vtp) -> return (nm, fid, vtp, vid)

getCurrentFid :: SubBlock -> Id
getCurrentFid (blc, path) = case (getBlock (blc, path)) of
	Nothing -> error "Unexpected error"
	Just (IFLoop _) -> getCurrentFid (blc, init path)
	Just (Funct _ fid _ _ _ _ _ _) -> fid

assignmentToVar :: (Block, Type) -> [Int] -> Maybe (Name, Id, Type, Id) -> Block
assignmentToVar (blc, tps) path (Just (nm, fid, vtp, vid)) = case (getBlock (blc, path)) of
	Nothing -> error "Unexpected error"
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

changeTreeNewVar :: Block -> [Int] -> Name -> Type -> Block
changeTreeNewVar blc [] nm tp = Funct (name blc) (idf blc) (retType blc) (tP blc) (byteCode blc) (Map.insert nm ((Map.size (varMap blc)), tp) (varMap blc)) ((maxVar blc)+1) (bl blc)
changeTreeNewVar blc (p:ath) nm tp = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewVar (blc2 !! p) ath nm tp) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewVar (blc3 !! p) ath nm tp) blc3) 

changeTreeNewBC :: Block -> [Int] -> [ByteCodeCommand] -> Block
changeTreeNewBC blc [] bcc = Funct (name blc) (idf blc) (retType blc) (tP blc) ((byteCode blc) ++ bcc) (varMap blc) (maxVar blc) (bl blc)
changeTreeNewBC blc (p:ath) bcc = case blc of
	(IFLoop blc2) -> IFLoop (set (element p) (changeTreeNewBC (blc2 !! p) ath bcc) blc2)
	(Funct n i r t bc vm mv blc3) -> Funct n i r t bc vm mv (set (element p) (changeTreeNewBC (blc3 !! p) ath bcc) blc3)

declarationTranslator :: Type -> Name -> Block -> [Int] -> Block
declarationTranslator tp nm blc path = case (findVar (blc, path) nm) of
	Nothing -> addVar (blc, path) nm tp
	Just _ -> error ("Can't declare variable " ++ show nm ++ " . It's already exist in this function!")

assignmentTranslator :: Block -> [Int] -> Int -> ConstMap -> Name -> Expression -> Block
assignmentTranslator blc path cf cm nm expr = assignmentToVar (expressionTranslator blc path cm expr) path (getVarInfo (blc, path) nm)

expressionTranslator :: Block -> [Int] -> ConstMap -> Expression -> (Block, Type)
expressionTranslator blc path cm expr = case expr of
	--Ident nm ->
	--Str s ->
	IntNumb inumb -> ((changeTreeNewBC blc path [(ILOAD inumb)]), "int")
	FloatNumb fnumb -> ((changeTreeNewBC blc path [(DLOAD fnumb)]), "double")
	-- FCall fname args -> case args of
	--UnarExpr un subexpr ->
	--BinarExpr bin subexpr1 subexpr2 ->

