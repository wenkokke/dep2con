{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Language.Structure.Dependency.Parse (pTree,pDeps) where

import           Control.Applicative ((<|>),(<*>),(<*),(*>))
import           Data.Char (isSpace)
import           Data.List (sort)
import           Language.POS (POS(POS))
import           Language.Structure.Dependency (Tree(..))
import           Language.Word (Word(Word))
import qualified Language.Word.Parse as W (pWord)
import           Text.ParserCombinators.UU ((<$>),pSome,pMany,pList1Sep)
import           Text.ParserCombinators.UU.BasicInstances (Parser,pSym,pMunch)
import           Text.ParserCombinators.UU.Utils (lexeme,pParens,pNatural,pLetter,pComma)


pTree :: Parser Tree
pTree = lexeme (pLeaf <|> pNode)
  where
    pLeaf :: Parser Tree
    pLeaf = Node <$> W.pWord <*> return []
    pNode :: Parser Tree
    pNode = pParens (Node <$> W.pWord <*> pMany pTree)


pDeps :: Parser Tree
pDeps = do pos     <- lexeme pInit
           depList <- pDepList pos
           return (depsToTree depList root)
  where
    pInit :: Parser (Int -> POS)
    pInit = (\tags serial -> tags !! (serial - 1)) <$> pPhrase
      where
        pWord   = pMunch (/='/')
        pPOS    = POS <$> pMunch (not . isSpace)
        pPhrase = pList1Sep (pSym ' ') (pWord *> pSym '/' *> pPOS)

    pDepList :: (Int -> POS) -> Parser [(Word, Word)]
    pDepList pos = pSome (lexeme pDep)
      where
        pDep   :: Parser (Word, Word)
        pDep   = pLabel *> pParens ((,) <$> pWord <* pComma <*> pWord)
          where
            pLabel             = pSome pLetter
            mkWord text serial = Word text (pos serial) serial
            pWord              = mkWord <$> pMunch (/='-') <* pSym '-' <*> pNatural

    depsToTree :: [(Word, Word)] -> Word -> Tree
    depsToTree deps word
      = Node word (sort . map (depsToTree deps . snd) . depsOf $ word)
      where
        depsOf :: Word -> [(Word, Word)]
        depsOf (Word _ _ i) = filter (\ (Word _ _ j , _) -> i == j) deps

    root :: Word
    root = Word "ROOT" (POS "ROOT") 0
