import qualified Language.Conversion.Dep2Con as Dep2Con (collins)
import qualified Language.Structure.Dependency.Parse as Dep (pTree)
import           Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main =
  print
    . Dep2Con.collins
    . runParser "stdin" Dep.pTree
    =<< getContents
