{-# LANGUAGE OverloadedStrings #-}
module Dictionary.Schema where

import Data.ByteString

data Słowo = Słowo { odmiana     :: ByteString
                   , formaBazowa :: ByteString
                   , morpho      :: Morpho
                   , kategoria   :: Maybe Kategoria
                   } deriving (Show, Read, Eq)

data Morpho = MorphoRzeczownik Rzeczownik
            | MorphoPrzymiotnik Przymiotnik
            | MorphoPrzysłówek Przysłówek
            | MorphoLiczebnik Liczebnik
            | MorphoZaimek Zaimek
            | MorphoCzasownik Czasownik
            | MorphoCzasownikTypuWinien CzasownikTypuWinien
            | MorphoPredykatyw Predykatyw
            | MorphoPrzyimek Przyimek
            | MorphoSpójnik Spójnik
            | MorphoKublik Kublik
            | MorphoCiałoObceNominalne CiałoObceNominalne
            | MorphoCiałoObceLuźne CiałoObceLuźne
            | MorphoWykrzyknienie Wykrzyknienie
            | MorphoBurkinostka Burkinostka
            deriving (Show, Read, Eq)

data Kategoria = Etnonim
               | Geograficzna
               | Imię
               | Nazwisko
               | Określenie
               | Osoba
               | Pospolita
               | Własna
               | Wydarzenie
               | Wytwór
               deriving (Show, Read, Eq)

--
-- Rodzaje odmian
--
data Liczba = Pojedyncza | Mnoga
            deriving (Show, Read, Eq)

data Przypadek = Mianownik
               | Dopełniacz
               | Celownik
               | Biernik
               | Narzędnik
               | Miejscownik
               | Wołacz
               deriving (Show, Read, Enum, Eq)

data Rodzaj = MęskiOsobowy 
            | MęskiZwierzęcy
            | MęskiRzeczowy
            | Żeński
            | NijakiZbiorowy
            | NijakiZwykły
            | PrzymnogiOsobowy
            | PrzymnogiZwykły
            | PrzymnogiOpisowy
            deriving (Show, Read, Eq)

data Osoba = Pierwsza
           | Druga
           | Trzecia
           deriving (Show, Read, Eq)

data Stopień = Równy
             | Wyższy
             | Najwyższy
             deriving (Show, Read, Eq)

data Aspekt = Niedokonany
            | Dokonany
            deriving (Show, Read, Eq)

data Zanegowanie = Zanegowana
                 | Niezanegowana
                 deriving (Show, Read, Eq)

data Akcentowość = Akcentowana
                 | Nieakcentowana
                 deriving (Show, Read, Eq)

data Poprzyimkowość = Poprzyimkowa
                    | Niepoprzyimkowa
                    deriving (Show, Read, Eq)

data Akomodacyjność = Uzgadniająca
                    | Rządząca
                    deriving (Show, Read, Eq)

data Aglutynacyjność = Aglutynacyjna
                     | Nieaglutynacyjna
                     deriving (Show, Read, Eq)

data Wokaliczność = Wokaliczna
                  | Niewokaliczna
                  deriving (Show, Read, Eq)

--
-- Części mowy
--
data Rzeczownik = Rzeczownik (Maybe Liczba) (Maybe Przypadek) Rodzaj
                | FormaDeprecjatywna Liczba (Maybe Przypadek) Rodzaj
                deriving (Show, Read, Eq)

data Przymiotnik = Przymiotnik (Maybe Liczba) (Maybe [Przypadek]) (Maybe [Rodzaj]) (Maybe Stopień)
                 | PrzymiotnikRrzyprzymiotnikowy
                 | PrzymiotnikPoprzyimkowy
                 deriving (Show, Read, Eq) 

data Przysłówek = Przysłówek (Maybe Stopień)
                deriving (Show, Read, Eq)

data Liczebnik = Liczebnik Liczba (Maybe [Przypadek]) (Maybe [Rodzaj]) (Maybe [Akomodacyjność])
               | LiczebnikH Stopień -- FIXME: hack na błędy w bazie
               deriving (Show, Read, Eq)

data Zaimek = ZaimekNietrzecioosobowy Liczba (Maybe Przypadek) (Maybe [Rodzaj]) Osoba (Maybe Akcentowość)
            | ZaimekTrzecioosobowy Liczba (Maybe Przypadek) (Maybe [Rodzaj]) (Maybe Akcentowość) (Maybe Poprzyimkowość)
            | ZaimekSiebie (Maybe Przypadek)
            deriving (Show, Read, Eq)

data Czasownik = CzasownikFormaNieprzeszła (Maybe Liczba) (Maybe Osoba) [Aspekt]
               | CzasownikFormaPrzyszłaByć (Maybe Liczba) (Maybe Osoba) Aspekt
               | CzasownikAglutynantByć (Maybe Liczba) (Maybe Osoba) Aspekt Wokaliczność
               | CzasownikPseudoimiesłów (Maybe Liczba) (Maybe [Rodzaj]) [Aspekt] (Maybe Aglutynacyjność)
               | CzasownikRozkaźnik (Maybe Liczba) (Maybe Osoba) [Aspekt]
               | CzasownikBezosobnik [Aspekt]
               | CzasownikBezokolicznik [Aspekt]
             
               | ImiesłówPrzysłówkowyWspółczesny Aspekt
               | ImiesłówPrzysłówkowyUprzedni Aspekt
               | CzasownikOdsłownik (Maybe Liczba) (Maybe [Przypadek]) Rodzaj [Aspekt] (Maybe Zanegowanie)
               | ImiesłówPrzymiotnikowyCzynny (Maybe Liczba) (Maybe [Przypadek]) (Maybe [Rodzaj]) [Aspekt] (Maybe Zanegowanie)
               | ImiesłówPrzymiotnikowyBierny (Maybe Liczba) (Maybe [Przypadek]) (Maybe [Rodzaj]) [Aspekt] (Maybe Zanegowanie)
               deriving (Show, Read, Eq)

data CzasownikTypuWinien = CzasownikTypuWinien (Maybe Liczba) (Maybe [Rodzaj]) Aspekt
                         deriving (Show, Read, Eq)

data Predykatyw = Predykatyw
                deriving (Show, Read, Eq)

data Przyimek = Przyimek Przypadek
              deriving (Show, Read, Eq)

data Spójnik = SpójnikWspółrzędny
             | SpójnikPodrzędny
             deriving (Show, Read, Eq)

data Kublik = Kublik
            deriving (Show, Read, Eq)

data CiałoObceNominalne = CiałoObceNominalne (Maybe Liczba) (Maybe Przypadek) Rodzaj
                        deriving (Show, Read, Eq)

data CiałoObceLuźne = CiałoObceLuźne
                    deriving (Show, Read, Eq)

data Wykrzyknienie = Wykrzyknienie
                   deriving (Show, Read, Eq)

data Burkinostka = Burkinostka
                 deriving (Show, Read, Eq)
