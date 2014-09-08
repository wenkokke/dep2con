{-# LANGUAGE FlexibleContexts #-}
module Language.Structure.Constituency.Parse where

import           Control.Monad.Supply (Supply, supply, evalSupply)
import           Language.POS (POS)
import           Language.POS.Parse (pPOS)
import qualified Language.Structure.Constituency as Con (Tree(..))
import           Language.Word (Word(..))
import           Language.Word.Parse (pText)
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils


newtype Index a = Index { runIndex :: Supply Int a }

evalIndex :: Index a -> a
evalIndex = flip evalSupply [1..] . runIndex

pTree :: Parser Con.Tree
pTree = evalIndex <$> ixTree
  where
    ixTree   = ixLeaf <|> ixNode
    ixLeaf   = mkLeaf <$> ixWord
    ixNode   = pParens $ mkNode <$> pPOS <*> ixForest
    ixForest = mkForest <$> pSome ixTree
    ixWord   = mkWord <$> pText <*> return mkIndex

    mkLeaf   = Index . fmap Con.Leaf . runIndex
    mkNode p = Index . fmap (Con.Node p) . runIndex
    mkWord t = Index . fmap (Word t) . runIndex
    mkIndex  = Index supply
    mkForest = Index . sequence . fmap runIndex
