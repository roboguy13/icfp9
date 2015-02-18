import VM.VM

import qualified Data.ByteString as B

main :: IO ()
main = B.getContents >>= runProgram

