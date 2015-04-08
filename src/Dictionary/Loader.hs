{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Dictionary.Loader where

import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as B

import Data.Word8
import Data.List

import System.IO
import Control.Monad (when)

getWord :: Handle -> IO Word8
getWord handle = B.head <$> B.hGet handle 1

binarySearch :: (Integral a, Monad m) => (a -> m Ordering) -> a -> a -> m (Maybe a)
binarySearch test imin imax
    | imax < imin = return Nothing
    | otherwise   = do
        let imid = (imin + imax) `div` 2
        res <- test imid
        case res of
            GT -> binarySearch test imin (imid - 1)
            LT -> binarySearch test (imid + 1) imax
            EQ -> return $ Just imid

padToLine :: Handle -> Integer -> IO ()
padToLine handle pos = do
    hSeek handle AbsoluteSeek pos
    ch <- getWord handle
    when (ch /= _lf) $ padToLine handle $ pos - 1

dictTest :: Handle -> B.ByteString -> Integer -> IO Ordering
dictTest handle word offset = do
    padToLine handle offset
    let testWord = \case
            []     -> return EQ
            (x:xs) -> do
                ch <- getWord handle
                case compare ch x of
                    EQ -> testWord xs
                    a  -> return a
    testWord $ B.unpack word

--
-- TODO: Nicer API for more than one search
-- TODO: Binary range extension (multiple results)
--
searchDict :: FilePath -> String -> IO ()
searchDict dict what = withBinaryFile dict ReadMode $ \handle -> do
    let needle = B.snoc (B.fromString what) _tab
    dictSize <- hFileSize handle
    res <- binarySearch (dictTest handle needle) 0 dictSize
    case res of
        Just offset -> do
            padToLine handle offset
            ln <- hGetLine handle
            print ln
        Nothing -> print "Nie znaleziono"

preprocess :: IO ()
preprocess = do
    original <- B.readFile "bin/dictionary"
    let result = B.intercalate "\n" $ sort $ init $ B.split _lf original
    B.writeFile "bin/processed" result
