module Language.Structure.Constituency where


import qualified Data.Tree as Rose
import Language.POS (POS)
import Language.Word (Word(Word))


data Tree
  = Leaf Word
  | Node POS [Tree]
  deriving (Eq, Show)


drawTree :: Tree -> String
drawTree = Rose.drawTree . go
  where
    go :: Tree -> Rose.Tree String
    go (Leaf word) = Rose.Node (show word) []
    go (Node pos rest) = Rose.Node (show pos) (map go rest)


index :: Tree -> Int
index (Leaf (Word _ i)) = i
index (Node _ rest) = minimum (map index rest)


instance Ord Tree where
  compare x y = compare (index x) (index y)
