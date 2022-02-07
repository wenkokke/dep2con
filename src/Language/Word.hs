module Language.Word (Word (..)) where

import Prelude hiding (Word)
import Language.POS (POS (..))
import Data.String.Utils (quoteString)

-- | Words are a combination of a textual string and an index into a
--   sentence.
data Word
   = Word { text   :: String
          , pos    :: POS
          , serial :: Int }
   deriving (Eq, Ord)

instance Show Word where
  showsPrec _ (Word txt pos ser) =
    showString (show txt) .
    showString "/" .
    showString (show pos) .
    showString "/" .
    shows ser
