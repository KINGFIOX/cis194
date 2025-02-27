-- Applicative parser for infix arithmetic expressions without any
-- dependency on hackage. Builds an explicit representation of the
-- syntax tree to fold over using client-supplied semantics.
module W05Parser (parseExp) where

import Control.Applicative (Alternative (..), optional)
import Control.Arrow (Arrow (first))
import Control.Monad (void)
import Control.Monad.State (evalStateT)
import Control.Monad.State.Lazy (StateT (..))
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')
import Data.Monoid

-- Expressions
data Expr = Const Integer | Add Expr Expr | Mul Expr Expr deriving (Show)

-- A parser threads some 'String' state through a computation that
-- produces some value of type @a@.
type Parser a = StateT String Maybe a

-- Parse one numerical digit.
digit :: Parser Integer
digit = StateT parseDigit -- parseDigit :: String -> Maybe (Integer, String)
  where
    parseDigit [] = Nothing
    parseDigit (c : cs)
      | isDigit c = Just (fromIntegral $ digitToInt c, cs)
      | otherwise = Nothing

-- Parse an integer. The integer may be prefixed with a negative sign.
num :: Parser Integer
num = maybe id (const negate) <$> optional (char '-') <*> (toInteger <$> some digit)
  where
    toInteger = foldl' ((+) . (* 10)) 0

-- Parse a single white space character.
space :: Parser ()
space = StateT parseSpace
  where
    parseSpace [] = Nothing
    parseSpace s@(c : cs)
      | isSpace c = Just ((), cs)
      | otherwise = Nothing

-- Consume zero or more white space characters.
eatSpace :: Parser ()
eatSpace = void (many space)

-- Parse a specific character.
char :: Char -> Parser Char
char c = StateT parseChar
  where
    parseChar [] = Nothing
    parseChar (x : xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing

-- Parse one of our two supported operator symbols.
op :: Parser (Expr -> Expr -> Expr)
op = (Add <$ char '+') <|> (Mul <$ char '*')

-- Succeed only if the end of the input has been reached.
eof :: Parser ()
eof = StateT parseEof
  where
    parseEof [] = Just ((), [])
    parseEof _ = Nothing

-- Parse an infix arithmetic expression consisting of integers, plus
-- signs, multiplication signs, and parentheses.
parseExpr :: Parser Expr
parseExpr =
  eatSpace *> ((buildOp <$> nonOp <*> (eatSpace *> op) <*> parseExpr) <|> nonOp)
  where
    buildOp x op y = x `op` y -- t -> (t -> t1 -> t2) -> t1 -> t2
    nonOp = char '(' *> parseExpr <* char ')' <|> Const <$> num

-- Run a parser over a 'String' returning the parsed value and the
-- remaining 'String' data.
execParser :: Parser a -> String -> Maybe (a, String)
execParser = runStateT

-- Run a parser over a 'String' returning the parsed value.
evalParser :: Parser a -> String -> Maybe a
evalParser = evalStateT

-- Parse an arithmetic expression using the supplied semantics for
-- integral constants, addition, and multiplication.
parseExp :: (Integer -> a) -> (a -> a -> a) -> (a -> a -> a) -> String -> Maybe a
parseExp con add mul = (convert <$>) . evalParser (parseExpr <* eof)
  where
    convert (Const x) = con x -- Expr -> a
    convert (Add x y) = add (convert x) (convert y)
    convert (Mul x y) = mul (convert x) (convert y)

-- 简单的计算器示例
calculate :: String -> Maybe Integer
calculate = parseExp id (+) (*)
