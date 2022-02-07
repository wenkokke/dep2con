{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

import qualified Language.Conversion.Dep2Con         as Dep2Con
import qualified Language.Structure.Constituency     as Con
import qualified Language.Structure.Dependency       as Dep
import qualified Language.Structure.Dependency.Parse as Dep
import           System.Console.CmdArgs
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils (runParser)



data Output
   = Default
   | ASCII
   | Markdown
   deriving (Data, Typeable)


data Options = Options
  { stanfordDependencies :: Bool
  , output               :: Output
  } deriving (Data, Typeable)


defaultOptions :: Options
defaultOptions = Options
  { stanfordDependencies = False
                        &= help "Parse Stanford-style dependencies."

  , output               = Default
                        &= help "Set output format (default, ASCII or markdown)"
  }
  &= summary "Dep2Con v1.0, (c) Wen Kokke 2014"
  &= program "dep2con"


main :: IO ()
main = do
  opts <- cmdArgs defaultOptions
  cont <- getContents

  let parser  :: Parser Dep.Tree
      parser  = if stanfordDependencies opts then Dep.pDeps else Dep.pTree
      depTree :: Dep.Tree
      depTree = runParser "StdIn" parser cont
      conTree = Dep2Con.collins depTree

  case output opts of

   Default  -> print conTree
   ASCII    -> putStrLn (Con.asASCII conTree)
   Markdown -> putStrLn (Con.asMarkdown conTree)
