-- Author: Colin Horne
-- Source: https://wiki.haskell.org/Phone_number
module Main where

import Data.Array
import System (getArgs)
import Data.Char (digitToInt, intToDigit, toLower, isLetter)
import Data.Foldable (foldl')
import Control.Monad (forM_)

translationsList = [
                 (0, "e"),
                 (1, "jnq"),
                 (2, "rwx"),
                 (3, "dsy"),
                 (4, "ft"),
                 (5, "am"),
                 (6, "civ"),
                 (7, "bku"),
                 (8, "lop"),
                 (9, "ghz")]


{--- Lookup functions for converting from/to digits ---}

numToChars :: Int -> String
numToChars = (!) arr
  where arr = array (0,9) translationsList

charToNum :: Char -> Int
charToNum = (!) arr
  where
    arr = array ('a','z') convertAll
    convert :: (Int, String) -> [(Char, Int)]
    convert (i,s) = map (\c -> (c, i)) s
    convertAll = concatMap convert translationsList



{--- The Trie data structure ---}

data Trie = Node [String] (Array Int Trie) | Empty

-- Add a node to the trie
insert node str = insert' str node $ map toLower $ filter isLetter str

insert' :: String -> Trie -> String -> Trie
insert' str (Node strs ts) [] = Node (str:strs) ts
insert' str (Node strs ts) (c:cs) =
  Node strs $! (ts // [(num, updatedNode)])
    where
      updatedNode = insert' str (ts ! num) cs
      num = charToNum c
insert' str Empty cs = insert' str (Node [] $ array (0,9) $ zip [0..9] $ repeat Empty) cs

readDict :: FilePath -> IO Trie
readDict file = fmap (trie . lines) $ readFile file

trie :: [String] -> Trie
trie strs = foldl' insert Empty strs


{--- Other functions ---}


-- Given a trie and a number, find the representations for each of
-- the number's prefixes, returning each result as a tuple containing
-- the prefix's representation, and its postfix
getAllStrings :: Trie -> [Int] -> [([String], [Int])]
getAllStrings Empty _ = []
getAllStrings (Node strs _)  []         = [(strs, [])]
getAllStrings (Node strs ts) xxs@(x:xs) = (getAllStrings (ts ! x) xs) ++ [(strs, xxs)]


-- Lists all string-representations of the given number
-- No further processing of this function's output is required
numToStrings :: Trie -> [Int] -> [String]
numToStrings trie num = map (dropWhile (== ' ')) $ loop [] True num
  where
    loop :: String -> Bool -> [Int] -> [String]
    loop prefix _ [] = [prefix]
    loop prefix allowDigit xxs@(x:xs) = case result of
      [] -> if allowDigit && noPartialMatch
            then loop (prefix++" "++[intToDigit x]) False xs
            else []
      xs -> xs
      where
        noPartialMatch = flip all allStrings $ (== []) . fst
        allStrings = getAllStrings trie xxs
        result = flip concatMap allStrings $
                 \(strs,rest) ->
                 flip concatMap strs $ \str ->
                 loop (prefix++" "++str) True rest



-- Reads a number from the given string,
-- returning a list of its digits
readNumber :: String -> [Int]
readNumber [] = []
readNumber (c:cs)
  | c >= '0' && c <= '9' = digitToInt c : rest
  | otherwise            = rest
    where rest = readNumber cs

main = do
     (dictFile:numbersFile:[]) <- getArgs
     dict <- readDict dictFile
     numbers <- fmap lines $ readFile numbersFile
     forM_ numbers $ \num' -> do
       let num = readNumber num'
       let strs = numToStrings dict num
       forM_ strs $ \str ->
         putStrLn $ num' ++ ": " ++ str
