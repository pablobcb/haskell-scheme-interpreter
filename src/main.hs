module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do
	args <- getArgs
	putStrLn (readExpr (args !! 0))


data Value = Atom String
           | List [Value]
           | DottedList [Value] Value
           | Number Integer
           | String String
           | Bool Bool 
           deriving Show


-- generaters a parser that checks if a
-- character exists in !#$%&|*+-/:<=>?@^_~
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


-- generaters a parser that checks if next
-- character is ' ' and skips it case true
spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser Value
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return (String x)


parseAtom :: Parser Value
parseAtom = do 
              first <- letter <|> symbol 
              rest <- many (letter <|> digit <|> symbol)
              let atom = first : rest :: [Char]
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom


parseNumber :: Parser Value
parseNumber = liftM (Number . read) (many1 digit)


parseExpr :: Parser Value
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr expr = case parsedExpr of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
    where parsedExpr = parse parseExpr "scheme" expr
  