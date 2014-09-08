{-# LANGUAGE FlexibleContexts #-}
module Language.Structure.Constituency.Parse (pTree) where

import           Control.Monad.Supply (Supply, supply, evalSupply)
import           Language.POS.Parse (pPOS)
import qualified Language.Structure.Constituency as Con (Tree(..))
import           Language.Word (Word(..))
import           Language.Word.Parse (pText)
import           Text.ParserCombinators.UU ((<$>),(<|>),(<*>),pSome)
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils (pParens)


newtype Index a = Index { runIndex :: Supply Int a }

evalIndex :: Index a -> a
evalIndex = flip evalSupply [1..] . runIndex

pTree :: Parser Con.Tree
pTree = evalIndex <$> pTree'
  where
    pTree'  = pLeaf <|> pNode
    pLeaf   = mkLeaf <$> pWord
    pNode   = pParens $ mkNode <$> pPOS <*> pForest
    pForest = mkForest <$> pSome pTree'
    pWord   = mkWord <$> pText <*> return mkIndex

    mkLeaf   = Index . (Con.Leaf <$>) . runIndex
    mkNode p = Index . (Con.Node p <$>) . runIndex
    mkWord t = Index . (Word t <$>) . runIndex
    mkIndex  = Index supply
    mkForest = Index . sequence . fmap runIndex
