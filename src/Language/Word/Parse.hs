{-# LANGUAGE FlexibleContexts #-}
module Language.Word.Parse (pWord) where

import Language.POS.Parse (pPOS)
import Language.Word (Word(..))
import Text.ParserCombinators.UU ((<?>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils (pQuotedString,pInteger)

-- | Parser for words.
pWord :: Parser Word
pWord = iI Word pQuotedString '/' pPOS '/' pInteger Ii <?> "Word"
