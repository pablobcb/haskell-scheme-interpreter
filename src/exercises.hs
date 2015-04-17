--exercise Parsing.1.1
--parseNumber = do
--  strNumber <- many1 digit
--  (return . Number . read) strNumber

-- exercise Parsing.1.2
-- parseNumber = (many1 digit) >>= (return . Number . read)
-- parseNumber = liftM (Number . read) (many1 digit)
