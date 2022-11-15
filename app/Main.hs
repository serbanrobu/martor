module Main where

import Parser (parse)
import Relude

main :: IO ()
main = getLine >>= print . parse
