import           Control.Monad (forM_)
import qualified Language.Structure.Constituency as Con (drawTree)
import qualified Language.Structure.Constituency.Parse as Con (pTree)
import           System.Environment (getArgs)
import           Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg -> do

    contents <- readFile arg
    let con = runParser arg Con.pTree contents
    putStrLn ("Printing " ++ arg)
    putStrLn (Con.drawTree con)
