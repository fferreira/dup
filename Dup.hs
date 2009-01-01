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

--
-- A simple duplicate file finder 
--

module Dup
  (
    findDuplicates
  , byFileName
  , getFilesFrom
  , getDuplicatesFrom
  , allFiles
  ) where

import System.FilePath ((</>), takeFileName)
import Dic (groupByKey)
import Control.Monad (forM)
import Control.Exception (handle)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)

findDuplicates :: (Eq a) => (FilePath -> IO(a, FilePath)) ->  ((a, FilePath) -> Bool) -> [FilePath] -> IO[[FilePath]]
findDuplicates info filterBy files = do
  fileInfo <- mapM info files
  return $ (filter ((>1) . length) . groupByKey) (filter filterBy fileInfo)

byFileName :: FilePath -> IO (FilePath, FilePath)
byFileName file = do 
  let pair = (takeFileName file, file)
  return pair

getFilesFrom :: FilePath -> IO[FilePath]
getFilesFrom topdir = do
  names <- getDirectoryContents' topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    symLinkStatus <- getSymbolicLinkStatus path
    if isDirectory && not(isSymbolicLink symLinkStatus)
      then getFilesFrom path
      else return [path]
  return (concat paths)


allFiles _ = True

-- A safe version of getDirectoryContents that returns [] when
-- the directory can't be opened
getDirectoryContents' :: FilePath -> IO[FilePath]
getDirectoryContents' files = handle (\_ -> return []) $ do
  getDirectoryContents files

getDuplicatesFrom :: (Eq a) => FilePath -> (FilePath -> IO(a, FilePath)) -> ((a, FilePath) -> Bool) -> IO[[FilePath]]
getDuplicatesFrom dir dupCriteria filterBy = do
  files <- getFilesFrom dir
  findDuplicates dupCriteria filterBy files

