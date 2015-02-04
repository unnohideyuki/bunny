module Main where
import Parser

main :: IO ()
main = getContents >>= print . parse
