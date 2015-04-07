{-# LANGUAGE TemplateHaskell #-}
module Dictionary.DeriveMorpho where

import Language.Haskell.TH
import Data.Attoparsec.Text
import Control.Applicative

deriveMorpho morpho parser = do
    TyConI (DataD _ _ _ dta _) <- reify morpho

    let parser' (NormalC cstr _) = appE (appE (varE 'fmap) (conE cstr)) (varE parser)
        alt' elem acc = infixE (Just elem) (varE '(<|>)) (Just acc)
    foldr alt' (varE 'empty) $ fmap parser' dta
