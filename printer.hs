
module Printer
(
) where

import Parser
import Translator
import Data.Word
import Data.Binary.Put
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Word
import Data.Binary
import qualified Data.List as List


--mainPrinter ::
--mainPrinter = 

main = do { c <- Prelude.getContents
;	case mainParser c of
		Left e -> do {
			Prelude.putStrLn "There is a syntax error in your program!"
		;	print e }
		Right r -> do {
			L.writeFile "C:\\Users\\1\\Desktop\\outp" (runPut (createBinary (createFile (mainTranslator r)))) }}


data FunctionHeader = FunctionHeader Int Int Int ByteString Int Int Int
data FunctionCode = FunctionCode FunctionHeader [ByteCodeCommand]
type Prog = [FunctionCode]
data Header = Header Int Int Int Int [ByteString] Int Int
type File = (Header, Prog)

createBinary :: File -> Put
createBinary ((Header s v cc cs cons ide cf), fcode) = do {
	putWord16le (fromIntegral (s) :: Word16)
;	putWord32le (fromIntegral (v) :: Word32)
;	putWord32le (fromIntegral (cc) :: Word32)
;	putWord32le (fromIntegral (cs) :: Word32)
;	mapM_ putByteString cons
;	putWord16le (fromIntegral (ide) :: Word16)
;	putWord32le (fromIntegral (cf) :: Word32)
;	mapM_ createBinaryFunct fcode }

createBinaryFunct :: FunctionCode -> Put
createBinaryFunct (FunctionCode (FunctionHeader fs fbc fsign signat fid cl ca) bcc) = do {
	putWord32le (fromIntegral (fs) :: Word32)
;	putWord32le (fromIntegral (fbc) :: Word32)
;	putWord32le (fromIntegral (fsign) :: Word32)
;	putByteString signat
;	putWord16le (fromIntegral (fid) :: Word16)
;	putWord32le (fromIntegral (cl) :: Word32)
;	putWord32le (fromIntegral (ca) :: Word32) 
;	mapM_ createBinaryBC bcc }

createBinaryBC :: ByteCodeCommand -> Put
createBinaryBC bcc = case bcc of
	INVALID -> putWord8 0
	DLOAD i -> (putWord8 1) >> (putByteString (BS.concat (L.toChunks (encode i))))
	ILOAD i -> (putWord8 2) >> (putWord64le (fromIntegral (i) :: Word64))
	SLOAD i -> (putWord8 3) >> (putWord16le (fromIntegral (i) :: Word16))
	DLOAD0 -> putWord8 4
	ILOAD0 -> putWord8 5
	SLOAD0 -> putWord8 6
	DLOAD1 -> putWord8 7
	ILOAD1 -> putWord8 8
	DLOADM1 -> putWord8 9
	ILOADM1 -> putWord8 10
	DADD -> putWord8 11
	IADD -> putWord8 12
	DSUB -> putWord8 13
	ISUB -> putWord8 14
	DMUL -> putWord8 15
	IMUL -> putWord8 16
	DDIV -> putWord8 17
	IDIV -> putWord8 18
	IMOD -> putWord8 19
	DNEG -> putWord8 20
	INEG -> putWord8 21
	IAOR -> putWord8 22
	IAAND -> putWord8 23
	IAXOR -> putWord8 24
	IPRINT -> putWord8 25
	DPRINT -> putWord8 26
	SPRINT -> putWord8 27
	I2D -> putWord8 28
	D2I -> putWord8 29
	S2I -> putWord8 30
	SWAP -> putWord8 31
	POP -> putWord8 32
	LOADDVAR0 -> putWord8 33
	LOADDVAR1 -> putWord8 34
	LOADDVAR2 -> putWord8 35
	LOADDVAR3 -> putWord8 36
	LOADIVAR0 -> putWord8 37
	LOADIVAR1 -> putWord8 38
	LOADIVAR2 -> putWord8 39
	LOADIVAR3 -> putWord8 40
	LOADSVAR0 -> putWord8 41
	LOADSVAR1 -> putWord8 42
	LOADSVAR2 -> putWord8 43
	LOADSVAR3 -> putWord8 44
	STOREDVAR0 -> putWord8 45
	STOREDVAR1 -> putWord8 46
	STOREDVAR2 -> putWord8 47
	STOREDVAR3 -> putWord8 48
	STOREIVAR0 -> putWord8 49
	STOREIVAR1 -> putWord8 50
	STOREIVAR2 -> putWord8 51
	STOREIVAR3 -> putWord8 52
	STORESVAR0 -> putWord8 53
	STORESVAR1 -> putWord8 54
	STORESVAR2 -> putWord8 55
	STORESVAR3 -> putWord8 56
	LOADDVAR i -> (putWord8 57) >> (putWord16le (fromIntegral (i) :: Word16))
	LOADIVAR i -> (putWord8 58) >> (putWord16le (fromIntegral (i) :: Word16))
	LOADSVAR i -> (putWord8 59) >> (putWord16le (fromIntegral (i) :: Word16))
	STOREDVAR i -> (putWord8 60) >> (putWord16le (fromIntegral (i) :: Word16))
	STOREIVAR i -> (putWord8 61) >> (putWord16le (fromIntegral (i) :: Word16))
	STORESVAR i -> (putWord8 62) >> (putWord16le (fromIntegral (i) :: Word16))
	LOADCTXDVAR i p -> (putWord8 63) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	LOADCTXIVAR i p -> (putWord8 64) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	LOADCTXSVAR i p -> (putWord8 65) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	STORECTXDVAR i p -> (putWord8 66) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	STORECTXIVAR i p -> (putWord8 67) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	STORECTXSVAR i p -> (putWord8 68) >> (putWord16le (fromIntegral (i) :: Word16)) >> (putWord16le (fromIntegral (p) :: Word16))
	DCMP -> putWord8 69
	ICMP -> putWord8 70
	JA i -> (putWord8 71) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPNE i -> (putWord8 72) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPE i -> (putWord8 73) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPG i -> (putWord8 74) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPGE i -> (putWord8 75) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPL i -> (putWord8 76) >> (putWord16le (fromIntegral (i) :: Word16))
	IFICMPLE i -> (putWord8 77) >> (putWord16le (fromIntegral (i) :: Word16))
	DUMP -> putWord8 78
	STOP -> putWord8 79
	CALL i -> (putWord8 80) >> (putWord16le (fromIntegral (i) :: Word16))
	CALLNATIVE i -> (putWord8 81) >> (putWord16le (fromIntegral (i) :: Word16))
	RETURN -> putWord8 82
	BREAK -> putWord8 83

createFile :: Program -> File
createFile (Program (rt, _) cf cm _) = ((informToHeader cf cm (constToBS cm)), (functionToFC [rt] []))

informToHeader :: Int -> ConstMap -> ([ByteString], Int) -> Header
informToHeader cf cm (bsc, size) = Header 47802 60315 (Map.size cm) size bsc 0 cf

functionToFC :: [Block] -> Prog -> Prog
functionToFC [] fc = fc
functionToFC blcs fc = case (List.head blcs) of
	(IFLoop blcs2) -> functionToFC (List.tail blcs) (functionToFC blcs2 fc)
	(Funct _ idf _ tpv bcc _ mv blcs3) -> functionToFC (List.tail blcs) (functionToFC blcs3 (fc ++ [convertFunction idf tpv bcc mv (sizeofBC bcc 0)]))

convertFunction :: Int -> (Maybe [String]) -> [ByteCodeCommand] -> Int -> (Int, Int, Int) -> FunctionCode
convertFunction idf Nothing bcc mv (_, _, size) = FunctionCode (FunctionHeader (size + 22) size 0 BS.empty idf mv 0) bcc
convertFunction idf (Just tpv) bcc mv (_, _, size) = FunctionCode (FunctionHeader (size + 22) size 0 BS.empty idf mv (List.length (tpv))) bcc

constToBS :: ConstMap -> ([ByteString], Int)
constToBS cm = convertStr (List.sortBy compConst (Map.toList cm)) [] 0

compConst :: (String, Int) -> (String, Int) -> Ordering
compConst (str1, i1) (str2, i2) = compare i1 i2

convertStr :: [(String, Int)] -> [ByteString] -> Int -> ([ByteString], Int)
convertStr [] bsl num = (bsl, num)
convertStr (s:l) bls num = convertStr l (bls ++ [pack((fst s) ++ "\0")]) (num + List.length(fst s) + 1)
