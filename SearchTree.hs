module SearchTree
  ( SearchTree
  , empty
  , Path (..)
  , mkSearchTree
  , search
  ) where



import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Function
import Control.Monad.Writer
import Data.Maybe



data SearchTree k v
  = Empty
  | Node
    (Maybe v) -- Value if we stop here
    (Map k (SearchTree k v)) -- Subtrees corresponding to additional keys in a path
  deriving (Eq, Show)

empty :: SearchTree k v -> Bool
empty Empty = True
empty _ = False



-- One path through the search tree: The keys along the way, and the final result
data Path k v = Path {keys :: [k], value :: v}

instance Eq k => Eq (Path k v) where
  (==) = (==) `on` (take 1 . keys)

instance Ord k => Ord (Path k v) where
  compare = compare `on` (take 1 . keys)

shorten :: Path k v -> Path k v
shorten path = path {keys = drop 1 $ keys path}



mkSearchTree :: (Ord k) => [Path k v] -> SearchTree k v
mkSearchTree [] = Empty
mkSearchTree paths = let
  paths' = sort paths
  in if null $ keys $ head paths'
    then Node
      (Just $ value $ head paths')
      (mkNodeMap $ tail paths')
    else Node
      Nothing
      (mkNodeMap paths')

mkNodeMap :: (Ord k) =>  [Path k v] -> Map k (SearchTree k v)
mkNodeMap = Map.fromList . map mkSubTree . group . filter (not . null . keys)

mkSubTree :: (Ord k) => [Path k v] -> (k, SearchTree k v)
mkSubTree paths = (head $ keys $ head paths, mkSearchTree $ map shorten paths)



searchNode :: (Ord k) => SearchTree k v -> k -> Writer [v] (SearchTree k v)
searchNode Empty k = return Empty
searchNode (Node mv NodeMap) k = do
  sequence_ $ tell . return <$> mv
  return $ fromMaybe Empty $ Map.lookup k NodeMap

search :: (Ord k) => SearchTree k v -> [k] -> [v]
search tree = execWriter . search' tree

search' :: (Ord k) => SearchTree k v -> [k] -> Writer [v] (SearchTree k v)
search' tree [] = return tree
search' tree (k:keys) = do
  tree' <- searchNode tree k
  if empty tree
    then return tree'
    else search' tree' keys