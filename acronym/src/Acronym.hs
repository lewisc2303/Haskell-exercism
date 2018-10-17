module Acronym (abbreviate) where

import Data.Char

--I want to map a funciton that looks for capital letter into the [Char]

abbreviate :: String -> String
abbreviate word = go word []
    where go word acr
            | length word == 0 = ""
            | isUpper (head word) == True = acr ++ ([head word]) ++ (go (tail word) acr)
            | otherwise = go (tail word) acr
                where wordHead = head word
