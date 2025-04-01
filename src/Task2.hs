-- The above pragma enables all warnings
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Task2 where

import Data.Char (isDigit)
import Data.Maybe (isJust)
import Task1 (Parse (..), parseInteger', splitWords)

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op
  = Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving (Show)

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving (Show)

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse = parseWords . splitWords
    where
      parseWords ss = parseWords' ss []

      getExpr (Just a) = Lit a
      getExpr Nothing = undefined

      getBinOp (Just op) = BinOp op
      getBinOp Nothing = undefined

      parseWords' (s : ss) stack | isJust sub = parseWords' ss (getExpr sub : stack)
        where
          sub = parse s
      parseWords' (s : ss) (l : r : es) | isJust sub = parseWords' ss (getBinOp sub r l : es)
        where
          sub = parse s
      parseWords' (s : ss) stack = parseWords' ss (Var s : stack)
      parseWords' [] [s] = Just s
      parseWords' _ _ = Nothing

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a

findVar :: [(String, a)] -> String -> Maybe a
findVar [] _ = Nothing
findVar ((s, v) : ss) f
  | s == f = Just v
  | otherwise = findVar ss f

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr _ (Lit l) = Just l
evalExpr vrs (Var v) = findVar vrs v
evalExpr vrs (BinOp op l r) = case (el, er) of
  (Just ul, Just ur) -> Just (evalBinOp op ul ur)
  _ -> Nothing
  where
    el = evalExpr vrs l
    er = evalExpr vrs r

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger var s = case parse s :: Maybe (Expr Integer IntOp) of
  Nothing -> Nothing
  Just e  -> evalExpr var e

instance Parse Integer where
  parse s
    | all isDigit s = Just (parseInteger' s)
    | otherwise = Nothing

instance Parse IntOp where
  parse s = case s of
    "+" -> Just Add
    "*" -> Just Mul
    "-" -> Just Sub
    _   -> Nothing

instance Eval Integer IntOp where
  evalBinOp op = case op of
    Add -> (+)
    Mul -> (*)
    Sub -> (-)

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'Reify' function is required to reconcile generic type
-- of intermediate 'Expr' expression with concrete type using 'a' and 'op'.
evaluate :: (Eval a op, Parse a, Parse op) => Reify a op -> [(String, a)] -> String -> Maybe a
evaluate reify m s = case parse s of
  Just e -> evalExpr m (reify e)
  Nothing -> Nothing

-- * Helpers

-- | Helper type for specifying 'Expr' with
-- concrete 'a' and 'op' in generic context
type Reify a op = Expr a op -> Expr a op

-- | Helper for specifying 'Expr' with 'Integer' and 'IntOp' in generic context
reifyInteger :: Reify Integer IntOp
reifyInteger = id
