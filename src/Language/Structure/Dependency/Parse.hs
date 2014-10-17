{-# LANGUAGE FlexibleContexts #-}
module Language.Structure.Dependency.Parse (pTree) where

import Control.Applicative ((<|>),(<*>))
import Data.List (sort)
import Language.Structure.Dependency (Tree(..))
import Language.Word (Word (..))
import Language.Word.Parse (pWord)
import Text.ParserCombinators.UU ((<$>),pMany)
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (lexeme,pParens)


pTree :: Parser Tree
pTree = lexeme (pLeaf <|> pNode)
  where
    pLeaf :: Parser Tree
    pLeaf = Node <$> pWord <*> return []
    pNode :: Parser Tree
    pNode = pParens (Node <$> pWord <*> pMany pTree)
