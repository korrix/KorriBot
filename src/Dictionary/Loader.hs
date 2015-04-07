module Dictionary.Loader where

import qualified Data.ByteString.Lazy as B

--test :: IO (Trie Int)
test = do
    dict <- B.readFile "dictionary"
    -- Słownik podzielony na linię. Ostatnia jest pomijana, bo jest pusta
    let words = map (B.takeWhile (/= 9)) $ init $ B.split 10 dict 
    let seekPos = zip [0..] words
    print $ last seekPos

    --print $ (seekPos !! 1000)
