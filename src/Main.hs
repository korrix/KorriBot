{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.Readline

import qualified Data.ByteString as B

import Dictionary.Parser
import Dictionary.Loader

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    maybeLine <- readline "KorriBot> "
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do addHistory line
                        process line
                        readEvalPrintLoop

process :: String -> IO ()
process line = putStrLn line

main :: IO ()
main = do
    putStrLn "Witaj w KorrBot"
    readEvalPrintLoop

--test :: IO ()
--test = do
--    tekst <- B.readFile "dictionary"
--    let słowa = B.split 10 tekst
--    let parsed = map parseEntry słowa

--    let myprint słowo (Left err) = do
--            B.putStr słowo
--            B.putStr "\t"
--            putStrLn err
--        myprint _ _ = return ()

--    mapM_ (uncurry myprint) $ zip słowa parsed
