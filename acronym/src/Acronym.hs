module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate word = go word [] ' '
    where go word acronym lstChar
            | length word == 0 = ""
            | (isUpper lstChar) && (isUpper currentChar) == True = go restOfWord acronym currentChar
            | ((lstChar == ' ') || (lstChar == '-')) == True = acronym ++ ([toUpper currentChar]) ++ (go restOfWord acronym currentChar)            
            | isUpper currentChar == True = acronym ++ ([head word]) ++ (go restOfWord acronym currentChar)
            | otherwise = go restOfWord acronym currentChar
                where currentChar = head word
                      restOfWord = tail word