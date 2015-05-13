module Scheme.ParserSpec () where
--runhaskell -isrc -itest /test/Scheme/ParserSpec.hs
import Scheme.Parser
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Parser.parseExpr" $ do

    describe "Parsing Numbers" $ do

      it "returns a (Number 42)::Value given \"42\"" $ do
        parseExpr "42" `shouldBe` (Number 42)


      it "returns a (Float 42.42)::Value given \"42.42\"" $ do
        parseExpr "42.42" `shouldBe` (Float 42.42)
    --    it "returns the first element of an *arbitrary* list" $
    --      property $ \x xs -> head (x:xs) == (x :: Int)
    --
    --    it "throws an exception if used with an empty list" $ do
    --      evaluate (head []) `shouldThrow` anyException