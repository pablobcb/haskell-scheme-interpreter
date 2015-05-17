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

    describe "with Numbers as arguments" $ do

      it "it returns (Number 42)::Value given \"42\" " $ do
        parseExpr "42" `shouldBe` (Number 42)


      it "it returns (Float 42.42)::Value given \"42.42\" " $ do
        parseExpr "42.42" `shouldBe` (Float 42.42)


      it "it returns (Number 24)::Value given \"#b101010\" " $ do
        parseExpr "#b101010" `shouldBe` (Number 24)


      it "it returns (Number 234)::Value given \"#d234\" " $ do
        parseExpr "#d234" `shouldBe` (Number 234)

      it "it returns (Number 5349)::Value given \"#o12345\" " $ do
        parseExpr "#o12345" `shouldBe` (Number 5349)


      it "it returns (Number 143)::Value given \"#x8F\" " $ do
        parseExpr "#x8F" `shouldBe` (Number 143)


    describe "with Strings as arguments" $ do

      it "it returns (String \"10\")::Value given \"10\" " $ do
        parseExpr "\"10\"" `shouldBe` (String "10")


      it "it returns (String \" with spaces \")::Value given \" with spaces \" " $ do
        parseExpr "\" with spaces \"" `shouldBe` (String " with spaces ")


