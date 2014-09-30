import           Language.POS (POS(..))
import qualified Language.Structure.Constituency as Con
import           Language.Structure.Conversion.Dep2Con
import qualified Language.Structure.Dependency as Dep
import qualified Language.Structure.Dependency.Parse as Dep (pTree)
import           Language.Word (Word(..))
import           Language.Word.Parse (pSentence)
import           System.Environment (getArgs)
import           Text.ParserCombinators.UU.Utils (runParser)

main :: IO ()
main = do
  args <- getArgs

  let tagFile = args !! 0
  let depFile = args !! 1
  tagContents <- readFile tagFile
  depContents <- readFile depFile

  let tags = runParser tagFile pSentence tagContents
  let tag :: Word -> POS
      tag (Word _ 0) = POS "ROOT"
      tag (Word _ i) = snd (tags !! (i - 1))

  let depTree  = runParser depFile Dep.pTree depContents
  let conTree  = collins tag depTree

  print conTree
