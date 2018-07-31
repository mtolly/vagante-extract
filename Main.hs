{- |
Extracts files from the game Vagante's data.vra.
Last updated for Vagante 1.011.3
1. zlib-decompress
2. every 10799 bytes (starting with byte 0), xor with 0xE5
3. skip 4 bytes (ED 00 00 00)
4. next 2 bytes are number of files - 1 (little endian)
5. then repeating pattern:
- length of filename (4 bytes little-endian)
- filename
- length of file data (4 bytes little-endian)
- file data
-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Binary.Get
import Data.Binary.Put
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import Control.Monad (forM, forM_)
import Codec.Compression.Zlib (decompress, compress)
import Data.Bits (xor)

-- | For each byte where position is divisible by 10799, xor it with 0xE5.
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
  bs <- obfuscate . decompress <$> BL.readFile vra
  let files = runGet splitFiles $ BL.drop 6 bs
  forM_ files $ \(name, file) -> do
    let out = dir </> BL8.unpack name
    putStrLn $ "Extracting " ++ out
    createDirectoryIfMissing True $ takeDirectory out
    BL.writeFile out file
  BL.writeFile (dir </> "repack-list.txt") $ BL8.unlines $ map fst files

main :: IO ()
main = getArgs >>= \case
  ["extract", vra, dir] -> extract vra dir
  ["archive", dir, vra] -> archive dir vra
  [x] -> case splitExtension x of
    (dir, ".vra") -> extract x dir
    _             -> archive x $ x <.> "vra"
  _ -> error "incorrect usage"

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
  files <- lines . BL8.unpack <$> BL.readFile (dir </> "repack-list.txt")
  mapM_ (putStrLn . ("  " ++)) files
  fileContents <- forM files $ \name -> do
    contents <- fmap BL.fromStrict $ B.readFile $ dir </> name
    return (BL8.pack name, contents)
  BL.writeFile vra $ compress $ obfuscate $ runPut $ do
    putWord32le 0xED
    putWord16le $ fromIntegral $ length files - 1
    joinFiles fileContents

joinFiles :: [(BL.ByteString, BL.ByteString)] -> Put
joinFiles = mapM_ $ \(name, file) -> do
  putWord32le $ fromIntegral $ BL.length name
  putLazyByteString name
  putWord32le $ fromIntegral $ BL.length file
  putLazyByteString file
