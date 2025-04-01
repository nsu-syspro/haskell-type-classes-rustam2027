{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where
import Data.Char (digitToInt)
import GHC.Unicode (isDigit)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit i) = i
evalIExpr (Add l r) = evalIExpr l + evalIExpr r
evalIExpr (Mul l r) = evalIExpr l * evalIExpr r

evalIExpr' :: Maybe IExpr -> Maybe Integer
evalIExpr' Nothing = Nothing
evalIExpr' (Just e) = Just (evalIExpr e)


-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a


data StringPair = StringPair String String
  deriving Show

left :: StringPair -> String
left (StringPair s _) = s

right :: StringPair -> String
right (StringPair _ s) = s

instance Semigroup StringPair where
  StringPair a b <> StringPair c d = StringPair (a <> c) (b <> d)

get :: String -> StringPair
get ""     = StringPair "" ""
get (x:xs) | x == ' '  = StringPair xs ""
           | otherwise = StringPair "" [x] <> get xs

parseInteger' :: String -> Integer
parseInteger' x = go x 0 where
  go "" acc = acc
  go (s:ss) acc = go ss (acc * 10 + toInteger (digitToInt s))

parseInteger :: String ->  IExpr
parseInteger = Lit . parseInteger'

splitWords :: String -> [String]
splitWords "" = []
splitWords x = right sub : splitWords (left sub) where
  sub = get x

parseWords :: [String] -> Maybe IExpr
parseWords ss = parseWords' ss []

parseWords' :: [String] -> [IExpr] -> Maybe IExpr
parseWords' (s:ss) stack    | all isDigit s = parseWords' ss (parseInteger s : stack)
parseWords' (s:ss) (l:r:es) | s == "+" = parseWords' ss (Add r l : es)
                            | s == "*" = parseWords' ss (Mul r l : es)
parseWords' [] [s] = Just s
parseWords' _ _    = Nothing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse = parseWords . splitWords


-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr x = evalIExpr' (parse x)
