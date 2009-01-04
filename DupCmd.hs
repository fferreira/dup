import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), hClose, hFileSize)
import Control.OldException (handle, bracket)
import qualified Data.ByteString.Lazy as DL
import qualified Data.ByteString as D
import Dup
import Data.Digest.Pure.MD5
import Data.Digest.Adler32

main = do
  args <- getArgs
  files <- getDuplicatesFrom (args !! 0) bySize nonEmptyFiles 
  files' <- mapM (findDuplicates byAdler allFiles) files 
  let dups = filter ((>0) . length) $ map (map unlines) files'
  prettyPrint $ map unlines dups


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

fileMD5_strict f = do
  wholeFile <- D.readFile f
  let content = DL.fromChunks [wholeFile]
  return $! md5 content

fileMD5 f = do
  content <- DL.readFile f
  return $! md5 content

--byAdler:: FilePath -> IO (GHC.Word.Word32, FilePath)
byAdler file = do
  chksum <- fileAdler file
  return (chksum, file)

fileAdler f = do
  content <- D.readFile f
  return $! adler32 (D.unpack content)


nonEmptyFiles (size, _) = size > 0
