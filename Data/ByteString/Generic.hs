module Data.ByteString.Generic where

import Prelude hiding (break, elem, putStr, getContents)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle, stdin, stdout)
import Data.String (IsString)
import Data.Monoid (Monoid)


class (Eq bs, Ord bs, Monoid bs, IsString bs) => ByteString bs where
  empty :: bs
  singleton :: Word8 -> bs
  pack :: [Word8] -> bs
  unpack :: bs -> [Word8]
  
  cons :: Word8 -> bs -> bs
  snoc :: bs -> Word8 -> bs
  append :: bs -> bs -> bs
  head :: bs -> Word8
  uncons :: bs -> Maybe (Word8, bs)
  last :: bs -> Word8
  tail :: bs -> bs
  init :: bs -> bs
  null :: bs -> Bool
  length :: bs -> Int
  
  map :: (Word8 -> Word8) -> bs -> bs
  reverse :: bs -> bs
  intersperse :: Word8 -> bs -> bs
  intercalate :: bs -> [bs] -> bs
  transpose :: [bs] -> [bs]
  
  foldl :: (a -> Word8 -> a) -> a -> bs -> a
  foldl' :: (a -> Word8 -> a) -> a -> bs -> a
  foldl1 :: (Word8 -> Word8 -> Word8) -> bs -> Word8
  foldl1' :: (Word8 -> Word8 -> Word8) -> bs -> Word8
  
  foldr :: (Word8 -> a -> a) -> a -> bs -> a
  foldr' :: (Word8 -> a -> a) -> a -> bs -> a
  foldr1 :: (Word8 -> Word8 -> Word8) -> bs -> Word8
  foldr1' :: (Word8 -> Word8 -> Word8) -> bs -> Word8
  
  concat :: [bs] -> bs
  concatMap :: (Word8 -> bs) -> bs -> bs
  any :: (Word8 -> Bool) -> bs -> Bool
  all :: (Word8 -> Bool) -> bs -> Bool
  maximum :: bs -> Word8
  minimum :: bs -> Word8
  
  scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> bs -> bs
  scanl1 :: (Word8 -> Word8 -> Word8) -> bs -> bs
  scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> bs -> bs
  scanr1 :: (Word8 -> Word8 -> Word8) -> bs -> bs
  
  mapAccumL :: (a -> Word8 -> (acc, Word8)) -> a -> bs -> (a, bs)
  mapAccumR :: (a -> Word8 -> (acc, Word8)) -> a -> bs -> (a, bs)
  
  replicate :: Int -> Word8 -> bs
  unfoldr :: (a -> Maybe (Word8, a)) -> a -> bs
  unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (bs, Maybe a)
  
  take :: Int -> bs -> bs
  drop :: Int -> bs -> bs
  splitAt :: Int -> bs -> (bs, bs)
  takeWhile :: (Word8 -> Bool) -> bs -> bs
  dropWhile :: (Word8 -> Bool) -> bs -> bs
  span :: (Word8 -> Bool) -> bs -> (bs, bs)
  spanEnd :: (Word8 -> Bool) -> bs -> (bs, bs)
  break :: (Word8 -> Bool) -> bs -> (bs, bs)
  breakByte :: Word8 -> bs -> (bs, bs)
  breakByte c bs = break (== c) bs
  breakEnd :: (Word8 -> Bool) -> bs -> (bs, bs)
  group :: bs -> [bs]
  groupBy :: (Word8 -> Word8 -> Bool) -> bs -> [bs]
  inits :: bs -> [bs]
  tails :: bs -> [bs]
  
  split :: Word8 -> bs -> [bs]
  splitWith :: (Word8 -> Bool) -> bs -> [bs]
  
  isPrefixOf :: bs -> bs -> Bool
  isSuffixOf :: bs -> bs -> Bool
  isInfixOf :: bs -> bs -> Bool

  breakSubstring :: bs -> bs -> (bs, bs)
  findSubstring :: bs -> bs -> Maybe Int
  findSubstrings :: bs -> bs -> [Int]
  
  elem :: Word8 -> bs -> Bool
  
  find :: (Word8 -> Bool) -> bs -> bs
  filter :: (Word8 -> Bool) -> bs -> bs
  partition :: (Word8 -> Bool) -> bs -> (bs, bs)
  
  index :: bs -> Int -> Word8
  elemIndex :: Word8 -> bs -> Maybe Int
  elemIndices :: Word8 -> bs -> [Int]
  elemIndexEnd :: Word8 -> bs -> Maybe Int
  findIndex :: (Word8 -> Bool) -> bs -> Maybe Int
  findIndices :: (Word8 -> Bool) -> bs -> Maybe Int
  count :: Word8 -> bs -> Int
  
  zip :: bs -> bs -> [(Word8, Word8)]
  zipWith :: (Word8 -> Word8 -> a) -> bs -> bs -> [a]
  unzip :: [(Word8, Word8)] -> (bs, bs)
  
  sort :: bs -> bs

  readFile :: FilePath -> IO bs
  writeFile :: FilePath -> bs -> IO ()
  appendFile :: FilePath -> bs -> IO ()
  
  hGetLine :: Handle -> IO bs
  hGetContents :: Handle -> IO bs
  hGet :: Handle -> Int -> IO bs
  hGetSome :: Handle -> Int -> IO bs
  hGetNonBlocking :: Handle -> Int -> IO bs
  hPut :: Handle -> bs -> IO ()
  hPutNonBlocking :: Handle -> bs -> IO bs
  hPutStrLn :: Handle -> bs -> IO ()
  
notElem :: (ByteString bs) => Word8 -> bs -> Bool
notElem c bs = not (elem c bs)
{-# INLINE notElem #-}

getLine :: (ByteString bs) => IO bs
getLine = hGetLine stdin

getContents :: (ByteString bs) => IO bs
getContents = hGetContents stdin

putStr :: (ByteString bs) => bs -> IO ()
putStr = hPut stdout

putStrLn :: (ByteString bs) => bs -> IO ()
putStrLn = hPutStrLn stdout

interact :: (ByteString bs) => (bs -> bs) -> IO ()
interact transformer = putStr . transformer =<< getContents

hPutStr :: (ByteString bs) => Handle -> bs -> IO ()
hPutStr = hPut

