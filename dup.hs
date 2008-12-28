-- Finding duplicate files
--

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>), splitFileName)
import System.Environment (getArgs)
import Data.List (sort, group)

getRecursiveContents :: FilePath -> IO[FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

findDuplicates origin = do
  files <- getRecursiveContents origin 
  return $ (filterDups .  map (snd . splitFileName)) files

-- filters duplicates in an orderable list
-- TODO optimize and write a function that doesn't need to order the list
filterDups :: (Eq a, Ord a) => [a] -> [a]
filterDups = map head . filter ((>1) . length) . group . sort

main = do
  args <- getArgs
  duplicates <- findDuplicates (args !! 0)
  putStrLn $ unlines duplicates 
