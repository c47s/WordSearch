module WordSearch where

import SearchTree
import Dictionary

import Control.Arrow
import Data.List
import Data.Char
import System.Environment



data Direction = U | UR | R | DR | D | DL | L | UL
  deriving (Show, Enum)

data DirWord k = DirWord Direction [k]
  deriving Show

foreword = DirWord R
backword = DirWord L

flipd :: Direction -> Direction
flipd U  = D
flipd UR = DL
flipd  R =  L
flipd DR = UL
flipd D  = U
flipd DL = UR
flipd  L =  R
flipd UL = DR

rotate :: Direction -> Direction
rotate UL = U
rotate dir = succ dir

lotate :: Direction -> Direction
lotate U = UL
lotate dir = pred dir

wordDir :: (Direction -> Direction) -> DirWord k -> DirWord k
wordDir f (DirWord dir ks) = DirWord (f dir) ks



type WordSearchTree k = SearchTree k (DirWord k)



mkWordSearchTree :: (Ord k) => [[k]] -> WordSearchTree k
mkWordSearchTree wordlist = mkSearchTree
  (  map (\keys -> Path keys $ foreword keys) wordlist
  ++ map (\keys -> Path (reverse keys) $ backword keys) wordlist)



rot :: [a] -> [a]
rot (x:xs) = xs ++ [x]

applyN :: Int -> (a -> a) -> a -> a
applyN n = foldl (.) id . replicate n

diags :: [[a]] -> [[a]]
diags xss = drop 1 . (\(xs,ys) -> ys ++ xs) . unzip $
  [ splitAt (length (head xss) - n + 1) xs
  | (n, xs) <- zip [0..]
    ( transpose
    [applyN n rot xs | (n, xs) <- zip [0..] xss]
    )
  ]
{-
coords from diags

(0, 0) -> (0, h)
(0, 1) -> (0, h-1)
(1, 1) -> (1, h)
(0, 2) -> (0, h-2)
(1, 2) -> (1, h-1)
(2, 2) -> (2, h)
y <= w - 1
(x, y) -> (x, h+x-y)
...
(0, 4) -> (1, 0)
(1, 4) -> (2, 1)
(2, 4) -> (3, 2)
(0, 5) -> (2, 0)
(1, 5) -> (3, 1)
y > w - 1
(x, y) -> (y+x-w+1, x)

a b c d
i j k l
w x y z
1 2 3 4

coords from diags . reverse

(0, 0) -> (0, 0)
(0, 1) -> (0, 1)
(1, 1) -> (1, 0)
(0, 2) -> (0, 2)
(1, 2) -> (1, 1)
(2, 2) -> (2, 0)
y <= h - 1
(x, y) -> (x, y-x)
...
(0, 4) -> (1, 3)
(1, 4) -> (2, 2)
(2, 4) -> (3, 1)
(0, 5) -> (2, 3)
(1, 5) -> (3, 2)
y > h - 1
(x, y) -> (x+y-h+1, h-x-1)

a b c d
i j k l
w x y z
1 2 3 4
-}



findWords2D :: (Ord k) => WordSearchTree k -> [[k]] -> [((Int, Int), DirWord k)]
findWords2D tree kss = findWords2DOrth tree kss ++ findWords2DDiag tree kss

findWords2DDiag :: (Ord k) => WordSearchTree k -> [[k]] -> [((Int, Int), DirWord k)]
findWords2DDiag tree kss
  = (
    map (\((x,y), vs)
      -> ( if y > length kss - 1
        then (x + y - length kss + 1, length kss - x - 1)
        else (x, y - x)
      , wordDir lotate vs
      )
    )
    . findWords2DHoriz tree
    . diags . reverse $ kss
  )
  ++ (
    map (\((x,y), vs)
      -> ( if y > length (head kss) - 1
        then (y + x - length (head kss) + 1, x)
        else (x, length kss + x - y)
      , wordDir rotate vs
      )
    )
    . findWords2DHoriz tree
    . diags $ kss
  )

findWords2DOrth :: (Ord k) => WordSearchTree k -> [[k]] -> [((Int, Int), DirWord k)]
findWords2DOrth tree kss =
    findWords2DHoriz tree kss
  ++
  (
    map (\((x,y), vs) -> ((y,x), wordDir (rotate . rotate) vs))
    . findWords2DHoriz tree
    . transpose $ kss
  )

findWords2DHoriz :: (Ord k) => WordSearchTree k -> [[k]] -> [((Int, Int), DirWord k)]
findWords2DHoriz tree
  = concatMap (map (\(y, (x, vs)) -> ((x,y), vs)))
  . zipWith (\i ks -> zip (repeat i) (findWords1D tree ks)) [0 .. ]

findWords1D :: (Ord k) => WordSearchTree k -> [k] -> [(Int, DirWord k)]
findWords1D tree
  = concat
  . zipWith (\x ks -> zip (repeat x) (search tree ks)) [0 .. ]
  . tails



printFoundWords :: [((Int, Int), DirWord Char)] -> String
printFoundWords = unlines . map
  $ intercalate "\t"
  . \((x, y), DirWord dir word) -> [word, show dir, show x, show y]



main = do
  args <- getArgs
  when $ null args $ error "Usage: WordSearch filename [words ...]"
  content <- readFile $ head args
  putStrLn . printFoundWords $ findWords2DOrth
    ( mkWordSearchTree $ if length args > 1
      then map (map toLower) args
      else filter ((>3) . length) dictionary
    )
    (lines $ toLower <$> content)
