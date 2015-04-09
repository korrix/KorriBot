{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Dictionary.Search ( searchDict ) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as B

import Data.Word8
import Data.List

import System.IO
import Control.Monad (liftM)

data Direction = Forward | Backward
               deriving (Show, Read, Eq)

goDir :: Enum a => Direction -> a -> a
goDir Forward  = succ
goDir Backward = pred

getWord8 :: Handle -> IO Word8
getWord8 handle = B.head <$> B.hGet handle 1

binarySearch :: (Integral a, Monad m) => a -> a -> (a -> m Ordering) -> m (Maybe a)
binarySearch imin imax test
    | imax < imin = return Nothing
    | otherwise   = do
        let imid = (imin + imax) `div` 2
        res <- test imid
        case res of
            GT -> binarySearch imin (imid - 1) test
            LT -> binarySearch (imid + 1) imax test
            EQ -> return $ Just imid

padToLine :: Direction -> Handle -> Integer -> Integer -> IO Integer
padToLine dir handle size pos = if pos < 1 || pos >= size - 1 then return pos else do
    hSeek handle AbsoluteSeek pos
    ch <- getWord8 handle
    if ch /= _lf then padToLine dir handle size $ goDir dir pos
                 else return pos

dictTest :: Handle -> B.ByteString -> IO Ordering
dictTest handle word = do
    let testWord = \case
            []     -> return EQ
            (x:xs) -> do
                ch <- getWord8 handle
                case compare ch x of
                    EQ -> testWord xs
                    a  -> return a
    testWord $ B.unpack word

extendBound :: Direction -> Handle -> Integer -> B.ByteString -> Integer -> IO Integer
extendBound dir handle size word offset = do 
    extra <- padToLine dir handle size offset
    res <- dictTest handle word
    if res == EQ then extendBound dir handle size word (goDir dir extra)
                 else return $ case dir of
                    Forward  -> extra
                    Backward -> offset + 2

searchDict :: FilePath -> String -> IO (Maybe B.ByteString)
searchDict dict what = withBinaryFile dict ReadMode $ \handle -> do
    let needle = B.snoc (B.fromString what) _tab
    dictSize <- hFileSize handle

    res <- binarySearch 0 dictSize $ \offset -> do
        padToLine Backward handle dictSize offset
        dictTest handle needle

    let fullMatch offset = do
            lower <- extendBound Backward handle dictSize needle offset
            upper <- extendBound Forward handle dictSize needle offset
            hSeek handle AbsoluteSeek lower
            B.hGet handle $ fromIntegral $ upper - lower

    case res of
        Just offset -> liftM return $ fullMatch offset
        Nothing     -> return Nothing

preprocess :: IO ()
preprocess = do
    original <- B.readFile "bin/dictionary"
    let result = B.intercalate "\n" $ sort $ init $ B.split _lf original
    B.writeFile "bin/processed" result
