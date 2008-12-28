{-    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


-- A simple duplicate file finder 
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
