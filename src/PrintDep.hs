import           Control.Monad (forM_)
import qualified Language.Structure.Dependency as Dep
import qualified Language.Structure.Dependency.Parse as Dep (pTree)
import           System.Environment (getArgs)
import           Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do

    contents <- readFile arg
    let dep = runParser arg Dep.pTree contents
    putStrLn ("Printing " ++ arg)
    putStrLn (Dep.drawTree dep)
