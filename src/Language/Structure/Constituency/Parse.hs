{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Language.Structure.Constituency.Parse (pTree) where

import Control.Applicative ((<$>),(<|>),(<*>))
import Language.POS (POS(POS))
import Language.Structure.Constituency (Tree(..))
import Language.Word (Word(Word))
import Text.ParserCombinators.UU (pSome)
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (lexeme,pParens,pLetter,pDigit,pAnySym)

pTree :: Parser Tree
pTree = snd . fix 1 <$> pTree'
  where
    pTree' :: Parser Tree
    pTree' = lexeme (pLeaf <|> pNode)
      where
        pText :: Parser String
        pText = lexeme . pSome
                $ pLetter <|> pDigit <|> pAnySym "$`',-.:"

        pPOS :: Parser POS
        pPOS = POS <$> pText

        pLeaf :: Parser Tree
        pLeaf = (\text -> Leaf (Word text (POS "") (-1))) <$> pText

        pNode :: Parser Tree
        pNode = pParens $ Node <$> pPOS <*> pSome pTree'


-- should really do this with a supply monad

fix :: Int -> Tree -> (Int, Tree)
fix i (Leaf (Word text pos _))          = (i + 1 , Leaf (Word text pos i))
fix i (Node pos [Leaf (Word text _ _)]) = (i + 1 , Leaf (Word text pos i))
fix i (Node pos children)               = let

  (i' , children') = fixChildren i children

  in (i' , Node pos children')

  where

    fixChildren :: Int -> [Tree] -> (Int, [Tree])
    fixChildren j []     = (j , [])
    fixChildren j (t:ts) = let

      (j'  , t' ) = fix j t
      (j'' , ts') = fixChildren j' ts

      in (j'' , t' : ts')
