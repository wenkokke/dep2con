{-# LANGUAGE FlexibleContexts #-}
module Language.POS.Parse (pPOS) where

import Language.POS (POS (..))
import Control.Applicative ((<$>))
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Utils (pQuotedString)


-- |Parser for part-of-speech tags.
pPOS :: Parser POS
pPOS = POS <$> pQuotedString
