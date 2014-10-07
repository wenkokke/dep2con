module Language.Word (Word (..)) where

import Language.POS (POS (..))

-- | Words are a combination of a textual string and an index into a
--   sentence.
data Word
   = Word { text   :: String
          , pos    :: POS
          , serial :: Int }
   deriving (Eq, Ord)

instance Show Word where
  showsPrec _ (Word txt pos ser) =
    showString (quoteString txt) .
    showString "/" .
    showString (quoteString (unPOS pos)) .
    showString "/" .
    shows ser

-- | Quote a string and escape quotes in the string.
quoteString :: String -> String
quoteString cs = '"' : quoteString' cs
  where
    quoteString' [] = ['"']
    quoteString' ('"' : cs) = '\\' : '"' : quoteString' cs
    quoteString' ( c  : cs) = c : quoteString' cs
