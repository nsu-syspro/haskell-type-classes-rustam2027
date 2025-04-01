{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- The above pragma enables all warnings

module Task3 where

import Task1 (Parse (..), splitWords)
import Task2 (Eval (..), Expr, evalExpr)

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
solveSAT :: String -> Maybe Bool
solveSAT s = case parsed of
  Nothing -> Nothing
  (Just p) -> Just (any (isTrue p) (getCombinations (getVariables s)))
  where
    parsed = parse s

isTrue :: Expr Bool BoolOp -> [(String, Bool)] -> Bool
isTrue x vrs = case evalExpr vrs x of
  Nothing -> False
  (Just b) -> b

getVariables :: String -> [String]
getVariables s = go (splitWords s) []
  where
    go ("and" : ss) list = go ss list
    go ("or" : ss) list = go ss list
    go ("xor" : ss) list = go ss list
    go (o : ss) list = go ss (o : list)
    go _ list = list

getCombinationsStarter :: [String] -> [(String, Bool)]
getCombinationsStarter = map (\x -> (x, False))

update :: [(String, Bool)] -> [(String, Bool)]
update [] = []
update (l : ls) = case l of
  (s, False) -> (s, True) : ls
  (s, True) -> (s, False) : update ls

getCombinations :: [String] -> [[(String, Bool)]]
getCombinations v = go (2 ^ length v) [getCombinationsStarter v]
  where
    go :: Integer -> [[(String, Bool)]] -> [[(String, Bool)]]
    go cnt ps@(p : _)
      | cnt == 0 = ps
      | otherwise = go (cnt - 1) (up : ps)
      where
        up = update p
    go _ _ = undefined

data BoolOp = And | Xor | Or
  deriving (Show)

instance Parse BoolOp where
  parse s = case s of
    "and" -> Just And
    "or" -> Just Or
    "xor" -> Just Xor
    _ -> Nothing

xor :: Bool -> Bool -> Bool
xor = (/=)

instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or = (||)
  evalBinOp Xor = xor

instance Parse Bool where
  parse s = case s of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
