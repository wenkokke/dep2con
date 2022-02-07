module Language.Structure.Binary where

import           Prelude hiding (Word)
import qualified Data.Tree as Rose
import           Language.POS (POS)
import           Language.Word (Word (Word))
import           Text.Printf (printf)


-- | Constituency trees are rose trees with POS tags in their nodes and words in
--   their leaves.
data Tree
  = Leaf Word
  | Node POS Tree Tree
  deriving (Eq)

instance Ord Tree where
  compare x y = compare (leftMostIndex x) (leftMostIndex y)

instance Show Tree where
  show (Leaf lbl)     = show lbl
  show (Node pos l r) = "(" ++ unwords (show pos : show l : show r : []) ++ ")"


-- |Get the left-most index from a constituency tree.
leftMostIndex :: Tree -> Int
leftMostIndex (Leaf (Word _ _ i)) = i
leftMostIndex (Node _ l r) = leftMostIndex l `min` leftMostIndex r


-- |Compute a node with the correct left/right.
node :: POS -> Tree -> Tree -> Tree
node pos x y =
  if leftMostIndex x < leftMostIndex y then Node pos x y else Node pos y x


-- |Convert a given tree to an instance of `Data.Tree` and draw it.
asASCII :: Tree -> String
asASCII = Rose.drawTree . go
  where
    go :: Tree -> Rose.Tree String
    go (Leaf word)    = Rose.Node (show word) []
    go (Node pos l r) = Rose.Node (show pos) (map go [l,r])


-- |Convert a given tree to a Markdown representation of it.
asMarkdown :: Tree -> String
asMarkdown (Leaf (Word txt _ _)) = show txt
asMarkdown (Node pos left right) = printf "[%s %s %s]" (show pos) (asMarkdown left) (asMarkdown right)


-- |Check if two trees are structurally equal.
(==^) :: Tree -> Tree -> Bool
(Leaf (Word _ _ i)) ==^ (Leaf (Word _ _ j)) = i == j
(Node _ l1 r1)      ==^ (Node _ l2 r2)      = l1 ==^ l2 && r1 ==^ r2
_                   ==^ _                   = False
