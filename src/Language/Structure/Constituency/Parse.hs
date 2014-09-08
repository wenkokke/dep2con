{-# LANGUAGE FlexibleContexts #-}
module Language.Structure.Constituency.Parse where

import           Control.Monad.Supply (Supply,supply,evalSupply)
import qualified Data.Tree as Rose
import           Language.POS (POS(..))
import           Language.POS.Parse (pRawPOS)
import qualified Language.Structure.Constituency as Con (Tree(..))
import           Language.Word (Word(..))
import           Language.Word.Parse (pRawWord)
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils


pTree :: Parser Con.Tree
pTree = flip evalSupply [1..] . rose2Con <$> pRoseTree

pRoseTree :: Parser (Rose.Tree String)
pRoseTree = pLeaf <|> pNode
  where
    pNode :: Parser (Rose.Tree String)
    pNode = pParens (Rose.Node <$> lexeme pRawPOS <*> pSome pRoseTree) <?> "Node"
    pLeaf :: Parser (Rose.Tree String)
    pLeaf = Rose.Node <$> lexeme pRawWord <*> return [] <?> "Word"

rose2Con :: Rose.Tree String -> Supply Int Con.Tree
rose2Con (Rose.Node raw []) = liftM (Con.Leaf . Word raw) supply
rose2Con (Rose.Node pos rest) = liftM (Con.Node (POS pos)) (mapM rose2Con rest)
