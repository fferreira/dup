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
  ) where

import System.FilePath ((</>), takeFileName)
import Dic (groupByKey)
import Control.Monad (forM)
import Control.Exception (handle)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink)

--findDuplicates :: (Eq a) => (FilePath -> IO(FilePath, a)) -> [FilePath] -> IO[[FilePath]]
findDuplicates info files = do
  fileInfo <- mapM info files
  return $ (filter ((>1) . length) . groupByKey) fileInfo

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

-- A safe version of getDirectoryContents that returns [] when
-- the directory can't be opened
getDirectoryContents' :: FilePath -> IO[FilePath]
getDirectoryContents' files = handle (\_ -> return []) $ do
  getDirectoryContents files

--getDuplicatesFrom :: FilePath -> (FilePath -> IO(FilePath, a)) -> IO[[FilePath]]
getDuplicatesFrom dir dupCriteria = do
  files <- getFilesFrom dir
  findDuplicates dupCriteria files

























--
-- Old Implementation
--
{-
getRecursiveContents :: FilePath -> IO[FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents' topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    symLinkStatus <- getSymbolicLinkStatus path
    if isDirectory && not(isSymbolicLink symLinkStatus)
      then getRecursiveContents path
      else return [path]
  return (concat paths)

fileNameAndSize :: FilePath -> IO (FilePath, Integer)
fileNameAndSize path = handle (\_ -> return (path, 0)) $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (path, size)

fileNameAndMD5 :: FilePath -> IO (FilePath, MD5Digest)
fileNameAndMD5 path = do
  chksum <- fileMD5 path
  return (path, chksum)


-- A safe version of getDirectoryContents that returns [] when
-- the directory can't be opened
getDirectoryContents' :: FilePath -> IO[FilePath]
getDirectoryContents' files = handle (\_ -> return []) $ do
  getDirectoryContents files

-- filters duplicates in an orderable list
-- TODO optimize and write a function that doesn't need to order the list
filterDups :: (Eq a, Ord a) => [a] -> [a]
filterDups = map head . filter ((>1) . length) . group . sort

--filterDupsBy :: (Eq a, Ord a) => (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> [a]
filterDupsBy eq ord = concat . filter ((>1) . length) . (groupBy eq) . (sortBy ord)

--isEqualSize :: (a, Integer) -> (a, Integer) -> Bool
isEqualSize (_, size1) (_, size2) = size1 == size2

--ordBySize :: (a, Integer) -> (a, Integer) -> Ordering 
ordBySize (_, size1) (_, size2) = compare size1 size2

fileMD5 f = do
  content <- D.readFile f
  return $ md5 content

compareMD5 f1 f2 = do
  m1 <- fileMD5 f1
  m2 <- fileMD5 f2
  return $ m1 == m2

oldmain = do
  args <- getArgs
  duplicates <- _findDuplicates (args !! 0)
  putStrLn $ duplicates 
-}


