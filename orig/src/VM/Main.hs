import VM.VM

import qualified Data.ByteString as B
import           System.Environment

main :: IO ()
main = do
  [fileName] <- getArgs
  file <- B.readFile fileName
  runProgram file

