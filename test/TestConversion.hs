{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative ((<$>),(<*>))
import           Control.Monad (when, forM_)
import           Data.IORef
import qualified Language.Conversion.Con2Bin as Con2Bin
import qualified Language.Conversion.Dep2Bin as Dep2Bin
import qualified Language.Conversion.Dep2Con as Dep2Con
import qualified Language.Structure.Binary             as Bin
import qualified Language.Structure.Constituency       as Con
import qualified Language.Structure.Constituency.Parse as Con
import qualified Language.Structure.Dependency         as Dep
import qualified Language.Structure.Dependency.Parse   as Dep
import           Paths_dep2con (getDataFileName)
import           System.Exit (exitSuccess,exitFailure)
import           System.FilePath.Glob (globDir1, compile)
import           Text.ParserCombinators.UU.BasicInstances (Parser)
import           Text.ParserCombinators.UU.Utils (runParser)



pTest :: Parser (Dep.Tree, Con.Tree)
pTest = (,) <$> Dep.pDeps <*> Con.pTree


main :: IO ()
main = do

  testDir   <- getDataFileName "test"
  testFiles <- globDir1 (compile "*.test") testDir
  hasFailed <- newIORef False

  forM_ testFiles $ \testFile -> do

    putStrLn $ "Running test in " ++ testFile
    (dep, con2) <- runParser "StdIn" pTest <$> readFile testFile

    let bin1 = Dep2Bin.collinsToledo dep
    let bin2 = Con2Bin.toledo dep con2
    let con1 = Dep2Con.collins dep

    when (not (bin1 Bin.==^ bin2)) $ do

      writeIORef hasFailed True
      putStrLn ("Expected: " ++ Con.asMarkdown con2)
      putStrLn ("Actual:   " ++ Con.asMarkdown con1)
      putStrLn ("Expected: " ++ Bin.asMarkdown bin2)
      putStrLn ("Actual:   " ++ Bin.asMarkdown bin1)

  hasFailed' <- readIORef hasFailed
  if hasFailed'
    then exitFailure
    else exitSuccess
