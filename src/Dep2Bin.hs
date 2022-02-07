{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

import qualified Language.Conversion.Dep2Con         as Dep2Con
import qualified Language.Conversion.Con2Bin         as Con2Bin
import qualified Language.Conversion.Dep2Bin         as Dep2Bin
import qualified Language.Structure.Binary           as Bin
import qualified Language.Structure.Dependency       as Dep
import qualified Language.Structure.Dependency.Parse as Dep
import           System.Console.CmdArgs
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils (runParser)


data Input
  = DefaultInput
  | Stanford
  deriving (Data, Typeable)



data Algorithm
  = DefaultAlgorithm
  | CollinsToledo
  | Collins
  | Toledo
  deriving (Data, Typeable)


data Output
   = DefaultOutput
   | ASCII
   | Markdown
   deriving (Data, Typeable)


data Options = Options
  { input     :: Input
  , algorithm :: Algorithm
  , output    :: Output
  } deriving (Data, Typeable)


defaultOptions :: Options
defaultOptions = Options
  { input     = DefaultInput
             &= help "Set input format (default, Stanford)."

  , algorithm = DefaultAlgorithm
             &= help "Set conversion algorithm (default, Collins-Toledo)."

  , output    = DefaultOutput
             &= help "Set output format (default, ASCII or Markdown)."
  }
  &= summary "Dep2Bin v1.0, (c) Wen Kokke 2014"
  &= program "dep2bin"


composition :: Dep.Tree -> Bin.Tree
composition dep = bin
  where
    con = Dep2Con.collins dep
    bin = Con2Bin.toledo dep con


main :: IO ()
main = do
  opts <- cmdArgs defaultOptions
  cont <- getContents

  let parser  :: Parser Dep.Tree
      parser  = case input opts of
                 DefaultInput -> Dep.pTree
                 Stanford     -> Dep.pDeps
      depTree :: Dep.Tree
      depTree = runParser "StdIn" parser cont
      binTree = case algorithm opts of
                 DefaultAlgorithm -> Dep2Bin.collinsToledo depTree
                 CollinsToledo    -> composition depTree

  case output opts of

   DefaultOutput -> print binTree
   ASCII         -> putStrLn (Bin.asASCII binTree)
   Markdown      -> putStrLn (Bin.asMarkdown binTree)
