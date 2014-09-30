module Language.Structure.Constituency where


import qualified Data.Tree as Rose
import Language.POS (POS)
import Language.Word (Word(Word))


data Tree
  = Leaf Word
  | Node POS [Tree]
  deriving (Eq)


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


instance Show Tree where
  show (Leaf lbl)      = show lbl
  show (Node pos rest) = "(" ++ unwords (show pos : map show rest) ++ ")"
