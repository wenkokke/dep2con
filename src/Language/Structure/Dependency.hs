module Language.Structure.Dependency where

import qualified Data.Tree as Rose
import Language.Word (Word(Word))


-- |Dependency trees are rose trees with words on both internal nodes
--  and leaves, and label on the links.
data Tree
  = Node
    { govenor    ::  Word
    , dependents :: [Tree]
    }
  deriving (Eq, Show)

instance Ord Tree where
  compare x y = compare (leftMostIndex x) (leftMostIndex y)


-- | Get the left-most index from a dependency tree.
leftMostIndex :: Tree -> Int
leftMostIndex (Node (Word _ _ i) deps) = minimum (i : map leftMostIndex deps)


-- | Convert the tree to an instance of `Data.Tree` and draw it.
drawTree :: Tree -> String
drawTree ct = Rose.drawTree (go ct)
  where
    go :: Tree -> Rose.Tree String
    go (Node gov deps) = Rose.Node (show gov) (map go deps)
