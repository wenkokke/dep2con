import qualified Language.Structure.Constituency as Con
import           Language.Structure.Conversion.Dep2Con (collins)
import qualified Language.Structure.Dependency as Dep
import qualified Language.Structure.Dependency.Parse as Dep (pTree)
import           Language.Word (Word (..))
import           Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main =
  print
    . collins
    . runParser "stdin" Dep.pTree
    =<< getContents
