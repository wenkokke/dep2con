module Language.POS where


-- |Part-of-speech tags are represented as simple strings.
newtype POS
      = POS String
      deriving (Eq, Ord)

instance Show POS where
  show (POS pos) = pos


-- |Conversion to a phrasal tag.
toXP :: POS -> POS
toXP (POS (x:_)) = POS (x : "P")

-- |Conversion to a bar tag.
toX' :: POS -> POS
toX' (POS (x:_)) = POS (x : "'")
