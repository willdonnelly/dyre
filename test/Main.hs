import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))
import System.Process (callCommand)

main :: IO ()
main = do
  setCurrentDirectory ("Tests")
  callCommand ("." </> "allTests.sh")
