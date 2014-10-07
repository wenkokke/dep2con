module Language.Structure.Constituency where

import qualified Data.Tree as Rose
import Language.POS (POS)
import Language.Word (Word (Word))


-- | Constituency trees are rose trees with POS tags in their nodes and words in
--   their leaves.
data Tree
  = Leaf Word
  | Node POS [Tree]
  deriving (Eq)

instance Ord Tree where
  compare x y = compare (leftMostIndex x) (leftMostIndex y)

instance Show Tree where
  show (Leaf lbl)      = show lbl
  show (Node pos rest) = "(" ++ unwords (show pos : map show rest) ++ ")"


-- | Get the left-most index from a constituency tree.
leftMostIndex :: Tree -> Int
leftMostIndex (Leaf (Word _ _ i)) = i
leftMostIndex (Node _ rest) = minimum (map leftMostIndex rest)


-- | Convert the tree to an instance of `Data.Tree` and draw it.
drawTree :: Tree -> String
drawTree = Rose.drawTree . go
  where
    go :: Tree -> Rose.Tree String
    go (Leaf word) = Rose.Node (show word) []
    go (Node pos rest) = Rose.Node (show pos) (map go rest)
