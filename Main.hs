{- |
Extracts files from the game Vagante's data.vra.
Last updated for Vagante 1.043.0 (beta)
1. zlib-decompress
2. skip 4 bytes (08 00 00 00)
3. next 2 bytes are "number of files - 3" (little endian)
  (used to be "files - 2" in 1.02, and "files - 1" in 1.01)
4. then repeating pattern:
- length of filename (4 bytes little-endian)
- filename
- length of file data (4 bytes little-endian)
- file data
-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Codec.Compression.Zlib     (compress, decompress)
import           Control.Applicative        (liftA3)
import           Control.Monad              (forM, forM_, void)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits                  (xor)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           System.Directory
import           System.Environment         (getArgs)
import           System.FilePath
import           System.Info                (os)
import           Text.Read                  (readMaybe)

-- | For each byte where position is divisible by 10799, xor it with 0xE5.
-- This appears to not be used in the format anymore.
obfuscate :: BL.ByteString -> BL.ByteString
obfuscate = BB.toLazyByteString . go where
  go bs = if BL.null bs
    then mempty
    else BB.word8 (BL.head bs `xor` 0xE5)
      <> BB.lazyByteString (BL.take 10798 $ BL.drop 1 bs)
      <> go (BL.drop 10799 bs)

-- | Extracts the contents of data.vra into a directory.
extract :: FilePath -> FilePath -> IO ()
extract vra dir = do
  bs <- decompress <$> BL.readFile vra
  let (magic, count, files) = flip runGet bs $ liftA3 (,,) getWord32le getWord16le splitFiles
      len = length files
      diff = len - fromIntegral count
  forM_ files $ \(name, file) -> do
    let out = dir </> BL8.unpack name
    putStrLn $ "Extracting " ++ out
    createDirectoryIfMissing True $ takeDirectory out
    BL.writeFile out file
  putStrLn $ "Magic number: " <> show magic
  putStrLn $ "Stated file count " <> show count <> ", found " <> show len <> " files"
  putStrLn $ case diff of
    3 -> "Difference matches Vagante 1.043 or similar"
    2 -> "Difference matches Vagante 1.02"
    1 -> "Difference matches Vagante 1.01 or older"
    _ -> "Unknown Vagante version"
  BL.writeFile (dir </> "repack-list.txt") $ BL8.intercalate (BL8.pack "\r\n")
    $ BL8.pack ("magic " <> show magic)
    : BL8.pack ("count " <> show count)
    : map fst files

main :: IO ()
main = getArgs >>= \case
  ["extract", vra, dir] -> extract vra dir
  ["archive", dir, vra] -> archive dir vra
  [x] -> case splitExtension x of
    (dir, ".vra") -> extract x dir
    _             -> archive x $ dropTrailingPathSeparator x <.> "vra"
  _ -> do
    putStrLn "*** vagante-extract ***"
    case os of
      "darwin" -> do
        putStrLn "Drag 'data.vra' onto 'drop-onto-me' to extract into 'data'."
        putStrLn "Then, drag 'data' onto 'drop-onto-me' to repack back into 'data.vra'."
      _ -> do
        putStrLn "Drag 'data.vra' onto 'vagante-extract.exe' to extract into 'data'."
        putStrLn "Then, drag 'data' onto 'vagante-extract.exe' to repack back into 'data.vra'."
    putStrLn "(press enter to close)"
    void getLine

splitFiles :: Get [(BL.ByteString, BL.ByteString)]
splitFiles = do
  eof <- isEmpty
  if eof
    then return []
    else do
      slen <- getWord32le
      s <- getLazyByteString $ fromIntegral slen
      flen <- getWord32le
      f <- getLazyByteString $ fromIntegral flen
      rest <- splitFiles
      return $ (s, f) : rest

-- | Collects the contents of a directory into a new data.vra.
archive :: FilePath -> FilePath -> IO ()
archive dir vra = do
  repackLines <- filter (/= "") . lines . filter (/= '\r') . BL8.unpack
    <$> BL.readFile (dir </> "repack-list.txt")
  let oldVersion = (0xED, fromIntegral $ length repackLines - 1, repackLines)
      (magic, count, files) = fromMaybe oldVersion $ do
        m <- listToMaybe repackLines          >>= stripPrefix "magic " >>= readMaybe
        c <- listToMaybe (drop 1 repackLines) >>= stripPrefix "count " >>= readMaybe
        return (m, c, drop 2 repackLines)
  mapM_ (putStrLn . ("Packing " ++)) files
  fileContents <- forM files $ \name -> do
    contents <- fmap BL.fromStrict $ B.readFile $ dir </> name
    return (BL8.pack name, contents)
  BL.writeFile vra $ compress $ runPut $ do
    putWord32le magic
    putWord16le count
    joinFiles fileContents
  putStrLn $ "Wrote to " ++ vra

joinFiles :: [(BL.ByteString, BL.ByteString)] -> Put
joinFiles = mapM_ $ \(name, file) -> do
  putWord32le $ fromIntegral $ BL.length name
  putLazyByteString name
  putWord32le $ fromIntegral $ BL.length file
  putLazyByteString file
