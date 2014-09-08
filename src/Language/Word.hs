module Language.Word where

-- |Words are a combination of a textual string and an index into a
--  sentence.
data Word
   = Word { text  :: String
          , index :: Int }
   deriving (Eq, Ord)

instance Show Word where
  showsPrec _ (Word str num) =
    showString str .
    showString "-" .
    shows      num
