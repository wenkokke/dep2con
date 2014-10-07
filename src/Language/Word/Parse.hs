{-# LANGUAGE FlexibleContexts #-}
module Language.Word.Parse (pWord) where

import Language.POS (POS (..))
import Language.POS.Parse (pPOS)
import Language.Word (Word(..))
import Control.Applicative ((<$>),(<|>))
import Text.ParserCombinators.UU (pSome,(<?>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (lexeme,pQuotedString,pNatural)

-- | Parser for words.
pWord :: Parser Word
pWord = iI Word pQuotedString '/' pPOS '/' pNatural Ii <?> "Word"
