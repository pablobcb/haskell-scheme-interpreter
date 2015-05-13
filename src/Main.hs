module Main where

import System.Environment
import Scheme.Parser

--TODO: negative complex numbers
main :: IO ()
main = do
    let values = [ "\"10\"", "\"iso \\n \"string\""
                 , "42.10", "42", "#b101010", "#d234", "#o12345", "#x8F"
                 , "'\n'", "' '", "'a'"
                 , "#t", "#f"
                 , "@symbol1" , "$ymbol2"
                 , "3+4i", "-2+2i"
                 , "(a list)", "(a (nested) list)", "(a (dotted . list) test)"
                 , "'\"joeo\"", "'123"
                 , "#(0 (2 5 1) \"Ana\")"
                 , "#[Error]: "
                 ]

    mapM (putStrLn . show . parseExpr) values

    return ()