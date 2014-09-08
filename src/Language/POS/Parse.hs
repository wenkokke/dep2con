{-# LANGUAGE FlexibleContexts #-}
module Language.POS.Parse (pPOS,pRawPOS) where

import Control.Applicative ((<$>),(<|>))
import Language.POS (POS(..))
import Text.ParserCombinators.UU (pAny,pSome,(<??>),(<?>))
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Utils (pSymbol,pUpper)


pPOS :: Parser POS
pPOS = POS <$> pRawPOS

-- |Parser for part-of-speech tags.
pRawPOS :: Parser String
pRawPOS = (pRegular <|> pSpecial) <?> "POS"
  where
    snoc x = (++[x])
    pRegular = pSome pUpper <??> (snoc <$> pSym '$')
    pSpecial = pAny pSymbol ["$","``","''","(",")",",","--",".",":"]
