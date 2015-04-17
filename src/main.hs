module Main where

import Control.Monad (liftM)
import Data.Array (Array (..), listArray)
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = do
    let values = ["1/20", "42.10", "42", "\"iso \\n \"string\"", "'\n'", "' '", "@asd", "32+2i"]
    mapM (putStrLn . readExpr ) values
    return ()

data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Bool Bool 
           | Character Char
           | Ratio Rational
           | Float Double
           | Complex (Complex Double)
           deriving Show


-- generaters a parser that checks if a
-- character exists in !#$%&|*+-/:<=>?@^_~
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


-- generaters a parser that checks if next
-- character is ' ' and skips it case true
spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser Char
escaped = do char '\\' 
             c <- oneOf ['\\', '\"', 'n', 'r', 't'] 
             return $ case c of 
               '\\' -> c
               '"'  -> c
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'


parseString :: Parser Value
parseString = do
                char '"'
                s <- many $ escaped <|> (noneOf ['\"', '\\']) <|> symbol
                char '"'
                return $ String s


--parseAtom :: Parser Value
--parseAtom = many (letter <|> digit <|> symbol) >>= return . Atom
parseAtom :: Parser Value
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

parseChar :: Parser Value
parseChar = do 
            char '\''
            c <- anyChar
            char '\''
            return $ Character c




parseBool :: Parser Value
parseBool = do
              char '#'
              (char 't' >> t) <|> (char 'f' >> f)
    where
        t = return $ Bool True
        f = return $ Bool False

parseFloat :: Parser Value
parseFloat = do 
    integerPart <- many1 digit
    char '.'
    decimalPart <- many1 digit
    let float = read (integerPart ++ ['.'] ++ decimalPart) :: Double
    return $ Float float

parseNumber :: Parser Value
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser Value
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser Value
parseRadixNumber = char '#' >>
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser Value
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser Value
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n

bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

parseOctal :: Parser Value
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n


parseHex :: Parser Value
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n


readWith f s = fst $ f s !! 0 

parseRatio :: Parser Value
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)



toDouble :: Value -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser Value
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x


parseExpr :: Parser Value
parseExpr = parseAtom
         <|> parseString
         <|> try parseChar
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseComplex
         <|> try parseNumber


readExpr :: String -> String
readExpr expr = case parsedExpr of
    Left err -> "No Match: " ++ show err
    Right val -> "Found Value: " ++ show val
    where parsedExpr = parse parseExpr "scheme" expr
  
