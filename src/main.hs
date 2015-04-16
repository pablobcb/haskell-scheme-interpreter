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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


-- generaters a parser that checks if next
-- character is ' ' and skips it case true
spaces :: Parser ()
spaces = skipMany1 space

escaped :: Parser Char
escaped = do char '\\' 
             x <- oneOf "\\\"nrt" 
             return $ case x of 
               '\\' -> x
               '"'  -> x
               'n'  -> '\n'
               'r'  -> '\r'
               't'  -> '\t'


parseString :: Parser Value
parseString = do
                char '"'
                s <- many $ escaped <|> (noneOf ['\"', '\\']) <|> symbol
                --s <- many $ noneOf ['"']
                char '"'
                return $ String s


parseAtom :: Parser Value
parseAtom = do 
              first <-  letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first : rest :: [Char]

              return $ case atom of 
                         _    -> Atom atom


parseBool :: Parser Value
parseBool = do
			  char '#'
			  (char 't' >> t) <|> (char 'f' >> f)
	where
		t = return $ Bool True
		f = return $ Bool False

parseNumber :: Parser Value
-- exercise Parsing.1.1
-- parseNumber = do
-- 				strNumber <- many1 digit
-- 				(return . Number . read) strNumber

-- exercise Parsing.1.2
-- parseNumber = (many1 digit) >>= (return . Number . read)

parseNumber = liftM (Number . read) (many1 digit)


parseExpr :: Parser Value
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
		 <|> parseBool

readExpr :: String -> String
readExpr expr = case parsedExpr of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
    where parsedExpr = parse parseExpr "scheme" expr
  