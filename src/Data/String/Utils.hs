module Data.String.Utils where

-- | Quote a string and escape quotes in the string.
quoteString :: String -> String
quoteString cs = '"' : quoteString' cs
  where
    quoteString' [] = ['"']
    quoteString' ('"' : cs) = '\\' : '"' : quoteString' cs
    quoteString' ( c  : cs) = c : quoteString' cs
