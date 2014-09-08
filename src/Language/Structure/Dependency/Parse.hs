{-# LANGUAGE FlexibleContexts #-}
module Language.Structure.Dependency.Parse where


import Language.Word (Word (..))
import Language.Word.Parse (pWord)
import Language.Structure.Dependency (Tree(..), Link(..), Label(..))
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances (Parser)
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.Utils


pTree :: Parser Tree
pTree = flip depsToTree (Word "ROOT" 0) <$> pDeps
  where
    pRel  :: Parser Label
    pRel  = Label <$> pSome pLetter
    pDep  :: Parser (Label, Word, Word)
    pDep  = iI (,,) pRel '(' (lexeme pWord) ',' (lexeme pWord) ')' Ii
    pDeps :: Parser [(Label, Word, Word)]
    pDeps = pSome (lexeme pDep)

    depsToTree :: [(Label, Word, Word)] -> Word -> Tree
    depsToTree deps g = Node g (toLink <$> depsOf g)
      where
        depsOf :: Word -> [(Label, Word, Word)]
        depsOf w = filter (\ (_, g, _) -> g == w) deps
        toLink :: (Label, Word, Word) -> Link
        toLink (r , _ , d) = Link r (depsToTree deps d)
