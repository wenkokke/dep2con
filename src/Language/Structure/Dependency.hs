module Language.Structure.Dependency where

import qualified Data.Tree as Rose
import Language.POS (POS)
import Language.Word (Word (..))


-- |Labels are labels on the links in a dependency tree.
newtype Label = Label String
              deriving (Eq)

instance Show Label where
  show (Label d) = d

-- |Links in a dependency tree allow for the edges between two nodes
--  to be labelled with labels.
data Link
  = Link
    { label     :: Label
    , dependent :: Tree
    }
  deriving (Eq, Show)


-- |Dependency trees are rose trees with words on both internal nodes
--  and leaves, and label on the links.
data Tree
  = Node
    { govenor   :: Word
    , subForest :: [Link]
    }
  deriving (Eq, Show)

drawTree :: Tree -> String
drawTree ct = Rose.drawTree (go ct)
  where
    go :: Tree -> Rose.Tree String
    go (Node gov deps) = Rose.Node (show gov) (map (go . dependent) deps)
