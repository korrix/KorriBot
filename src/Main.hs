module Main where

import Dictionary.Parser (test)

import System.Console.Readline

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

--main :: IO ()
--main = do
--    putStrLn "Witaj w KorrBot"
--    readEvalPrintLoop

main = test
