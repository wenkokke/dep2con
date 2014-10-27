{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

import qualified Language.Conversion.Dep2Bin         as Dep2Bin (toledo)
import qualified Language.Structure.Binary           as Bin (drawTree)
import qualified Language.Structure.Dependency       as Dep (Tree)
import qualified Language.Structure.Dependency.Parse as Dep (pTree,pDeps)
import           System.Console.CmdArgs
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils (runParser)


data Options = Options
  { stanfordDependencies :: Bool
  , drawTree             :: Bool
  } deriving (Data, Typeable)


defaultOptions :: Options
defaultOptions = Options
  { stanfordDependencies = False
                        &= help "Parse Stanford-style dependencies."

  , drawTree             = False
                        &= help "Print output as an ASCII tree."
  }
  &= summary "Dep2Con v1.0, (c) Pepijn Kokke 2014"
  &= program "dep2con"


main :: IO ()
main = do
  opts <- cmdArgs defaultOptions
  cont <- getContents

  let parser  :: Parser Dep.Tree
      parser  = if stanfordDependencies opts then Dep.pDeps else Dep.pTree
      depTree = runParser "StdIn" parser cont
      binTree = Dep2Bin.toledo depTree

  -- fugly haskell
  putStrLn
    $ (
      if drawTree opts
      then Bin.drawTree
      else show
      )
    binTree
