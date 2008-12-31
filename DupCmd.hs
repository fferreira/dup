import System.Environment (getArgs)
import Dup


main = do
  args <- getArgs
  files <- getDuplicatesFrom ".." byFileName
  let dups = map unlines files
  prettyPrint dups


--prettyPrint :: [String] -> IO()
prettyPrint files = do
  putStrLn "Duplicate Files\n"
  mapM putStrLn files
