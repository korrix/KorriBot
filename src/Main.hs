module Main where

import System.Console.Readline
import Text.Show.Pretty

import qualified Data.ByteString as B

import Dictionary.Parser
import Dictionary.Search

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
    maybeLine <- readline "KorriBot> "
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do addHistory line
                        process line
                        readEvalPrintLoop

query :: String -> IO ()
query word = do
    q <- searchDict "bin/processed" word
    case q of
        Just match -> do
            putStrLn $ ppShow $ parseEntry match
        Nothing -> putStrLn "Nie znaleziono podanego słowa"


process :: String -> IO ()
process line = case words line of
    ("query":word:xs) -> query word
    [] -> return ()
    _ -> putStrLn "Nieznana komenda / niepoprawne wywołanie"

main :: IO ()
main = do
    putStrLn "Witaj w KorrBot"
    readEvalPrintLoop
