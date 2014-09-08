{-# LANGUAGE FlexibleContexts #-}
module Language.POS.Parse (pPOS) where

import Control.Applicative ((<$>),(<|>))
import Language.POS (POS(..))
import Text.ParserCombinators.UU (pAny,pSome,(<??>),(<?>))
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)
import Text.ParserCombinators.UU.Utils (lexeme, pSymbol,pUpper)


-- |Parser for part-of-speech tags.
pPOS :: Parser POS
pPOS = lexeme $ POS <$> pRawPOS
  where
    pRawPOS :: Parser String
    pRawPOS = (pRegular <|> pSpecial) <?> "POS"
      where
        snoc x = (++[x])
        pRegular = pSome pUpper <??> (snoc <$> pSym '$')
        pSpecial = pAny pSymbol ["$","``","''","(",")",",","--",".",":"]
