{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TemplateHaskell        #-}

module Dictionary.Parser where

import Prelude hiding (readFile, putStrLn, putStr)

import Data.Text hiding (map, foldr, empty)
import Data.Text.IO

import Data.Char (isAlphaNum)

import Data.Attoparsec.Text
import Control.Applicative

import qualified Dictionary.Schema as S
import Dictionary.DeriveMorpho

--
-- Helpers
--
voluntary :: Parser a -> Parser (Maybe a)
voluntary p = Just <$> p <|> pure Nothing

(-=>) :: Text -> b -> Parser b
(-=>) s c = (string s >> return c)

class HasParser a where
    parser :: Parser a

instance HasParser a => HasParser [a] where
    parser = parser `sepBy` (char '.')

-- Helper automatycznie dopasowujący parser po typie pola
data Voluntary
data Obligatory

type family FieldProp a where
    FieldProp (Maybe a) = Voluntary
    FieldProp a         = Obligatory

class FieldParser flag a where
    field :: FieldProp a ~ flag => Parser a

instance HasParser a => FieldParser Obligatory a where
    field = char ':' *> parser

instance HasParser a => FieldParser Voluntary (Maybe a) where
    field = voluntary (char ':' *> parser)

-- Helper automatycznie dopasowujący parser do pól konstruktora wartości
class ConstructorParser a b where
    cparse' :: Text -> Parser a -> Parser b

instance (FieldParser (FieldProp a) a, ConstructorParser b c) => ConstructorParser (a -> b) c where
    cparse' text constr = cparse' text $ constr <*> field

instance ConstructorParser a a where
    cparse' text constr = string text >> constr

cparse text constr = cparse' text (pure constr)

--
-- Parser słownika
--
słownikParser :: Parser [S.Słowo]
słownikParser = many (słowoParser <* endOfLine)

--
-- Parser słowa
--
słowoParser :: Parser S.Słowo
słowoParser = do
    let wyraz = takeWhile1 (/= '\t') <* char '\t' <?> "Wyraz"
    odmiana     <- wyraz
    formaBazowa <- wyraz
    morpho      <- parser
    kategoria   <- voluntary (char '\t' *> parser)
    return (S.Słowo odmiana formaBazowa morpho kategoria) <?> "Słowo"

-- Automatyczne dostarczanie parsera dla morpho

--deriveMorpho = appE (appE (varE 'fmap) (conE 'S.MorphoPrzymiotnik)) (varE 'parser)

instance HasParser S.Morpho where
    parser = $(deriveMorpho ''S.Morpho 'parser)
          <?> "Morpho"

instance HasParser S.Kategoria where
    parser =  "etnonim"      -=> S.Etnonim
          <|> "geograficzna" -=> S.Geograficzna
          <|> "imię"         -=> S.Imię
          <|> "nazwisko"     -=> S.Nazwisko
          <|> "określenie"   -=> S.Określenie
          <|> "osoba"        -=> S.Osoba
          <|> "pospolita"    -=> S.Pospolita
          <|> "własna"       -=> S.Własna
          <|> "wydarzenie"   -=> S.Wydarzenie
          <|> "wytwór"       -=> S.Wytwór
          <?> "Kategoria"

--
-- Parsery odmian
--
instance HasParser S.Liczba where
    parser =  "sg" -=> S.Pojedyncza
          <|> "pl" -=> S.Mnoga
          <?> "Liczba"

instance HasParser S.Przypadek where
    parser =  "nom"  -=> S.Mianownik 
          <|> "gen"  -=> S.Dopełniacz 
          <|> "dat"  -=> S.Celownik 
          <|> "acc"  -=> S.Biernik 
          <|> "inst" -=> S.Narzędnik 
          <|> "loc"  -=> S.Miejscownik 
          <|> "voc"  -=> S.Wołacz 
          <?> "Przypadek"

instance HasParser S.Rodzaj where
    parser =  "m1" -=> S.MęskiOsobowy
          <|> "_"  -=> S.MęskiOsobowy -- FIXME. Prosty hack na błędy w bazie 
          <|> "m2" -=> S.MęskiZwierzęcy
          <|> "m3" -=> S.MęskiRzeczowy
          <|> "f"  -=> S.Żeński
          <|> "n1" -=> S.NijakiZbiorowy
          <|> "n2" -=> S.NijakiZwykły
          <|> "p1" -=> S.PrzymnogiOsobowy
          <|> "p2" -=> S.PrzymnogiZwykły
          <|> "p3" -=> S.PrzymnogiOpisowy
          <?> "Rodzaj"

instance HasParser S.Osoba where
    parser =  "pri" -=> S.Pierwsza
          <|> "sec" -=> S.Druga
          <|> "ter" -=> S.Trzecia
          <?> "Osoba"

instance HasParser S.Stopień where
    parser =  "pos"  -=> S.Równy
          <|> "comp" -=> S.Wyższy
          <|> "sup"  -=> S.Najwyższy
          <?> "Stopień"

instance HasParser S.Aspekt where
    parser =  "imperf" -=> S.Niedokonany
          <|> "perf"   -=> S.Dokonany
          <?> "Aspekt"

instance HasParser S.Zanegowanie where
    parser =  "aff" -=> S.Niezanegowana
          <|> "neg" -=> S.Zanegowana
          <?> "Zanegowanie"

instance HasParser S.Akcentowość where
    parser =  "akc"  -=> S.Akcentowana
          <|> "nakc" -=> S.Nieakcentowana
          <?> "Akcentowość"

instance HasParser S.Poprzyimkowość where
    parser =  "praep"  -=> S.Poprzyimkowa
          <|> "nopraep" -=> S.Niepoprzyimkowa
          <?> "Poprzyimkowość"

instance HasParser S.Akomodacyjność where
    parser =  "congr" -=> S.Uzgadniająca
          <|> "rec"   -=> S.Rządząca
          <?> "Akomodacyjność"

instance HasParser S.Aglutynacyjność where
    parser =  "agl"  -=> S.Aglutynacyjna
          <|> "nagl" -=> S.Nieaglutynacyjna
          <?> "Aglutynacyjność"

instance HasParser S.Wokaliczność where
    parser =  "wok"  -=> S.Wokaliczna
          <|> "nwok" -=> S.Niewokaliczna 
          <?> "Wokaliczność"

--
-- Parsery części mowy
--
instance HasParser S.Rzeczownik where
    parser =  cparse "subst" S.Rzeczownik
          <|> cparse "depr" S.FormaDeprecjatywna
          <?> "Rzeczownik"

 
instance HasParser S.Przymiotnik where
    parser =  cparse "adj"  S.Przymiotnik
          <|> cparse "adja" S.PrzymiotnikRrzyprzymiotnikowy
          <|> cparse "adjp" S.PrzymiotnikPoprzyimkowy
          <?> "Przymiotnik"

instance HasParser S.Przysłówek where
    parser =  cparse "adv" S.Przysłówek
          <?> "Przysłówek"

instance HasParser S.Liczebnik where
    parser =  cparse "num" S.Liczebnik
          <|> cparse "num" S.LiczebnikH
          <?> "Liczebnik"

instance HasParser S.Zaimek where
    parser =  cparse "ppron12" S.ZaimekNietrzecioosobowy
          <|> cparse "ppron3"  S.ZaimekTrzecioosobowy
          <|> cparse "siebie"  S.ZaimekSiebie
          <?> "Zaimek"

instance HasParser S.Czasownik where
    parser =  cparse "fin"    S.CzasownikFormaNieprzeszła
          <|> cparse "bedzie" S.CzasownikFormaPrzyszłaByć
          <|> cparse "aglt"   S.CzasownikAglutynantByć
          <|> cparse "praet"  S.CzasownikPseudoimiesłów
          <|> cparse "impt"   S.CzasownikRozkaźnik
          <|> cparse "imps"   S.CzasownikBezosobnik
          <|> cparse "inf"    S.CzasownikBezokolicznik
          <|> cparse "pcon"   S.ImiesłówPrzysłówkowyWspółczesny
          <|> cparse "pant"   S.ImiesłówPrzysłówkowyUprzedni
          <|> cparse "ger"    S.CzasownikOdsłownik
          <|> cparse "pact"   S.ImiesłówPrzymiotnikowyCzynny
          <|> cparse "ppas"   S.ImiesłówPrzymiotnikowyBierny
          <?> "Czasownik"

instance HasParser S.CzasownikTypuWinien where
    parser =  cparse "winien" S.CzasownikTypuWinien
          <?> "CzasownikTypuWinien"

instance HasParser S.Predykatyw where
    parser =  cparse "pred" S.Predykatyw
          <?> "Predykatyw"

instance HasParser S.Przyimek where
    parser =  cparse "prep" S.Przyimek
          <?> "Przyimek"

instance HasParser S.Spójnik where
    parser =  cparse "conj" S.SpójnikWspółrzędny
          <|> cparse "comp" S.SpójnikPodrzędny
          <?> "Spójnik"

instance HasParser S.Kublik where
    parser =  cparse "qub" S.Kublik
          <?> "Kublik"

instance HasParser S.CiałoObceNominalne where
    parser =  cparse "xxs" S.CiałoObceNominalne
          <?> "CiałoObceNominalne"

instance HasParser S.CiałoObceLuźne where
    parser =  cparse "xxx" S.CiałoObceLuźne
          <?> "CiałoObceLuźne"

instance HasParser S.Wykrzyknienie where
    parser = cparse "interj" S.Wykrzyknienie
          <?> "Wykrzyknienie"

instance HasParser S.Burkinostka where
    parser = cparse "burk" S.Burkinostka
          <?> "Burkinostka"

test :: IO ()
test = do
    tekst <- readFile "small"
    let słowa = splitOn "\n" tekst
    let parsed = map (parseOnly słowoParser) słowa

    let myprint słowo (Left err) = do
            putStr słowo
            putStr "\t"
            putStrLn $ pack err
        myprint _ _ = return ()

    mapM_ (uncurry myprint) $ Prelude.zip słowa parsed
    --print $ parseOnly słownikParser s
