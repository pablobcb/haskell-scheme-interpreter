module Scheme.ParserSpec (main, spec) where
--runhaskell -isrc -itest /test/Scheme/ParserSpec.hs
import Scheme.Parser
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "when applying Parser.parseExpr" $ do

    describe "with Numbers as arguments, it returns" $ do

      it "(Number 42)::Value given \"42\"" $ do
        parseExpr "42" `shouldBe` (Number 42)


      it "(Float 42.42)::Value given \"42.42\"" $ do
        parseExpr "42.42" `shouldBe` (Float 42.42)
    --    it "returns the first element of an *arbitrary* list" $
    --      property $ \x xs -> head (x:xs) == (x :: Int)
    --
    --    it "throws an exception if used with an empty list" $ do
    --      evaluate (head []) `shouldThrow` anyException