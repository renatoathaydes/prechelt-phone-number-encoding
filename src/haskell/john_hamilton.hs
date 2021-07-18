-- Author: John Hamilton
-- Source: https://wiki.haskell.org/Phone_number

import Data.Char
import Data.List
import Data.Map (fromListWith, findWithDefault)
import System.Environment

encodeWord = map f . filter (/= '"') where
  f x = head $ [d | (s, d) <- ps, (toLower x) `elem` s]
  ps = zip ["e", "jnq", "rwx", "dsy", "ft", "am", "civ", "bku", "lop", "ghz"]
           ['0'..'9']

translate wordMap _ "" = [""]
translate wordMap digit xs@(x:xs') =
  if all null ys && digit
    then combine [[x]] (translate wordMap False xs')
    else concat $ zipWith combine ys zs
  where
    ys = [findWithDefault [] s wordMap | s <- (tail . inits) xs]
    zs = [translate wordMap True s | s <- tails xs']
    combine [] _ = []
    combine ys [""] = ys
    combine ys zs = [y ++ " " ++ z | y <- ys, z <- zs]

process wordMap n = [n ++ ": " ++ x | x <- xs] where
  xs = translate wordMap True $ filter (`notElem` "-/") n

main = do
  [dictionary, input] <- getArgs
  words <- readFile dictionary
  let wordMap = fromListWith (++) [(encodeWord w, [w]) | w <- lines words]
  numbers <- readFile input
  mapM_ putStrLn $ lines numbers >>= process wordMap
