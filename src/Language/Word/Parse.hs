{-# LANGUAGE FlexibleContexts #-}
module Language.Word.Parse where

import Language.POS (POS(..))
import Language.POS.Parse (pPOS)
import Language.Word (Word(..))
import Control.Applicative ((<$>),(<|>))
import Text.ParserCombinators.UU (pSome,(<?>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (lexeme,pLetter,pDigit,pDot,pNatural)

-- |Parser for words.
pWord :: Parser Word
pWord = lexeme $ iI Word pText '-' pNatural Ii <?> "Word"

-- |Parser for text.
pText :: Parser String
pText = pSome (pLetter <|> pDigit) <|> (return <$> pDot) <?> "Text"

-- |Parser for tagged words.
pTagged :: Parser (String, POS)
pTagged = lexeme $ iI (,) pText '/' pPOS Ii

-- |Parser for part-of-speech tagged phrases.
pSentence :: Parser [(Word, POS)]
pSentence = zipWith go [1..] <$> pSome pTagged
  where
    go :: Int -> (String, POS) -> (Word, POS)
    go ix (raw, pos) = (Word raw ix, pos)
