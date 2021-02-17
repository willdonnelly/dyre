import System.Directory (setCurrentDirectory)
import System.Process (callCommand)

main :: IO ()
main = do
  setCurrentDirectory "Tests"
  callCommand "sh allTests.sh"
