module Language.Structure.Constituency where

import           Prelude hiding (Word)
import           Data.Monoid ((<>))
import qualified Data.Tree as Rose
import           Language.POS (POS)
import           Language.Word (Word (Word, serial))


-- |Constituency trees are rose trees with POS tags in their nodes and words in
--  their leaves.
data Tree
  = Leaf Word
  | Node POS [Tree]
  deriving (Eq)

instance Ord Tree where
  compare x y = compare (leftMostIndex x) (leftMostIndex y)

instance Show Tree where
  show (Leaf lbl)      = show lbl
  show (Node pos rest) = "(" ++ unwords (show pos : map show rest) ++ ")"


-- |Get the first part-of-speech tag in a tree.
topMostPOS :: Tree -> POS
topMostPOS (Leaf (Word _ pos _)) = pos
topMostPOS (Node pos _)          = pos


-- |Get the left-most index from a constituency tree.
leftMostIndex :: Tree -> Int
leftMostIndex (Leaf (Word _ _ i)) = i
leftMostIndex (Node _ rest) = minimum (map leftMostIndex rest)


-- |Get all words in a constituency tree.
allWords :: Tree -> [Word]
allWords (Leaf word)  = return word
allWords (Node _ children) = concatMap allWords children


-- |Compute the minimum distance from any node in the (sub-)tree to a
--  specific serial. /O(n)/
minDistance :: Int -> Tree -> Int
minDistance i = minimum . map (abs . (i -) . serial) . allWords


-- |Compare two trees, first by their distance to their governor, and
--  second by their index in the sentence (from the left). /O(n+m)/
nearestThenLeftMost :: Int -> Tree -> Tree -> Ordering
nearestThenLeftMost i x y = byDistanceToGovernor <> byIndex
  where
    byDistanceToGovernor = minDistance i x `compare` minDistance i y
    byIndex              = leftMostIndex x `compare` leftMostIndex y



-- |Convert the tree to an instance of `Data.Tree` and draw it.
asASCII :: Tree -> String
asASCII = Rose.drawTree . go
  where
    go (Leaf word)         = Rose.Node (show word) []
    go (Node pos children) = Rose.Node (show pos) (map go children)


-- |Convert a given tree to a Markdown representation of it.
asMarkdown :: Tree -> String
asMarkdown (Leaf (Word txt _ _)) = show txt
asMarkdown (Node pos children)   = "[" ++ unwords (show pos : map asMarkdown children) ++ "]"
