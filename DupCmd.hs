import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), hClose, hFileSize)
import Control.Exception (handle, bracket)
import qualified Data.ByteString.Lazy as D
import Dup
import MD5


main = do
  args <- getArgs
  files <- getDuplicatesFrom (args !! 0) bySize nonEmptyFiles 
  files' <- findDuplicates byMD5 allFiles (concat files) 
  let dups = map unlines files'
  prettyPrint dups


--prettyPrint :: [String] -> IO()
prettyPrint files = do
  putStrLn "Duplicate Files\n"
  mapM putStrLn files

--bySize :: FilePath -> IO (FilePath, Integer)
bySize path = handle (\_ -> return (0, path)) $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (size, path)

byMD5:: FilePath -> IO (MD5Digest, FilePath)
byMD5 path = do
  chksum <- fileMD5 path
  return (chksum, path)

fileMD5 f = do
  content <- D.readFile f
  return $ md5 content

nonEmptyFiles (size, _) = size > 0
