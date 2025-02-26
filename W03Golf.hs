module W03Golf where

import Data.Map.Strict (Map, empty, insertWith)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)

-- * Task 1

skips :: [a] -> [[a]]
skips [] = []
skips x@(_ : xs) = x : skips xs

-- * Task 2

localMaxima :: [Integer] -> [Integer]
localMaxima (a : xs@(b : c : arr)) --
  | a < b && b > c = b : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []

-- * Task 3

-- print True to '*', False to ' '
p :: Bool -> Char
p True = '*'
p _ = ' '

-- calc max count, then map l to the max count,
-- then add all the lines together.
histogram :: [Integer] -> String
histogram x =
  unlines
    ( map
        -- print line
        ( \i ->
            map
              -- get '*' or ' ' from the Map line by line,
              -- `fromMaybe 0 (M.lookup j m)` lookup the Map with 0 as default
              (\j -> p $ fromMaybe 0 (M.lookup j m) >= i)
              -- char by char, for each line
              [0 .. 9]
        )
        -- from top to bottom
        $ reverse [1 .. M.foldr max 0 m {- get max count -}]
    )
    -- suffix
    ++ "==========\n0123456789\n"
  where
    -- construct the Map, key: number, value: occurrencies/frequencies
    -- empty Map
    -- insertWith :: (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
    m = foldl (\m k -> insertWith (+) k 1 m) empty x