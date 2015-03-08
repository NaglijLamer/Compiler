
module Constant
(
	ConstMap
,	constAnalyzer
) where

import Parser
import qualified Data.Map.Strict as Map
import Data.Maybe

--Type of map for constants.
type ConstMap = Map.Map String Int

--Function to start analysis.
constAnalyzer :: [Statement] -> ConstMap
constAnalyzer stm = bodyAnalyzer stm Map.empty

--Analysis of statements.
bodyAnalyzer :: [Statement] -> ConstMap -> ConstMap
bodyAnalyzer [] cm = cm
bodyAnalyzer ((Assignment _ expr):xs) cm = bodyAnalyzer xs (expressionAnalyzer expr cm)
bodyAnalyzer ((Function _ _ _ (Left natnm)):xs) cm = bodyAnalyzer xs cm
bodyAnalyzer ((Function _ _ _ (Right body)):xs) cm = bodyAnalyzer xs (bodyAnalyzer body cm)
bodyAnalyzer ((ReturnSt (Just expr)):xs) cm = bodyAnalyzer xs (expressionAnalyzer expr cm)
bodyAnalyzer ((PrintSt (Just args)):xs) cm = bodyAnalyzer xs (argsAnalyzer args cm)
bodyAnalyzer ((WhileLoop expr body):xs) cm = bodyAnalyzer xs (bodyAnalyzer body (expressionAnalyzer expr cm))
bodyAnalyzer ((ForLoop _ _ _ body):xs) cm = bodyAnalyzer xs (bodyAnalyzer body cm)
bodyAnalyzer ((IfElse expr body bodyelse):xs) cm = bodyAnalyzer xs (bodyAnalyzer (fromMaybe [] bodyelse) (bodyAnalyzer body (expressionAnalyzer expr cm)))
bodyAnalyzer ((FunctionCall _ args):xs) cm = bodyAnalyzer xs (argsAnalyzer (fromMaybe [] args) cm)
bodyAnalyzer (_:xs) cm = bodyAnalyzer xs cm

--Analysis of expressions.
expressionAnalyzer :: Expression -> ConstMap -> ConstMap
expressionAnalyzer expr cm = case expr of
	UnarExpr _ subexpr -> expressionAnalyzer subexpr cm
	BinaryExpr _ subexpr1 subexpr2 -> expressionAnalyzer subexpr2 (expressionAnalyzer subexpr1 cm)
	Str s -> if (Map.member s cm)
		then cm
		else Map.insert s ((Map.size cm) + 1) cm
	FCall _ args -> argsAnalyzer (fromMaybe [] args) cm
	_ -> cm

--Analysis of list of arguments.
argsAnalyzer :: [Expression] -> ConstMap -> ConstMap
argsAnalyzer [] cm = cm
argsAnalyzer (x:xs) cm = argsAnalyzer xs (expressionAnalyzer x cm)
