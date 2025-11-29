{-# LANGUAGE OverloadedStrings #-}
module SemanticsSpec (spec) where

import Test.Hspec
import Gram.Validate
import Gram.Parse (parseGram)
import Gram.CST (Identifier(..), Symbol(..))
import Data.Either (isLeft, isRight)
import Text.Megaparsec (parse, errorBundlePretty)

-- Helper to parse and validate
validateSource :: String -> Either [ValidationError] ()
validateSource input = 
  case parse parseGram "test" input of
    Left _ -> Left [] -- Should not happen in these tests
    Right gram -> validate gram

-- Helper to extract error type
isDuplicateDefinition :: ValidationError -> Bool
isDuplicateDefinition (DuplicateDefinition _) = True
isDuplicateDefinition _ = False

isUndefinedReference :: ValidationError -> Bool
isUndefinedReference (UndefinedReference _) = True
isUndefinedReference _ = False

isSelfReference :: ValidationError -> Bool
isSelfReference (SelfReference _) = True
isSelfReference _ = False

spec :: Spec
spec = do
  describe "Basic Pattern Validation" $ do
    it "accepts a single valid definition" $ do
      validateSource "[a]" `shouldSatisfy` isRight

    it "accepts multiple unique definitions" $ do
      validateSource "[a], [b]" `shouldSatisfy` isRight

    it "rejects duplicate definitions" $ do
      let result = validateSource "[a], [a]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isDuplicateDefinition
        _ -> expectationFailure "Expected single DuplicateDefinition error"

    it "accepts forward references" $ do
      validateSource "[b | a], [a]" `shouldSatisfy` isRight
    
    it "accepts backward references" $ do
      validateSource "[a], [b | a]" `shouldSatisfy` isRight

    it "rejects undefined references" $ do
      let result = validateSource "[a | b]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isUndefinedReference
        _ -> expectationFailure "Expected single UndefinedReference error"

    it "rejects direct self-reference" $ do
      let result = validateSource "[a | a]"
      result `shouldSatisfy` isLeft
      case result of
        Left [err] -> err `shouldSatisfy` isSelfReference
        _ -> expectationFailure "Expected single SelfReference error"

    it "accepts indirect cycles" $ do
      validateSource "[a | b], [b | a]" `shouldSatisfy` isRight
