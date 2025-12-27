{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module GramHs.CLI.JSON
  ( patternToJSON
  , errorToJSON
  , Meta(..)
  , PatternResult(..)
  , Diagnostics(..)
  , ErrorResponse(..)
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Key (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToZonedTime, utc)
import System.IO.Unsafe (unsafePerformIO)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy as BSL
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Generics (Generic)

data Meta = Meta
  { metaVersion :: T.Text
  , metaCommand :: T.Text
  , metaTimestamp :: T.Text
  , metaHash :: T.Text
  } deriving (Generic, Show)

instance ToJSON Meta where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

data Diagnostics = Diagnostics
  { diagnosticsTimeMs :: Double
  , diagnosticsMemoryBytes :: Integer
  } deriving (Generic, Show)

instance ToJSON Diagnostics where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 12 }

data PatternResult = PatternResult
  { resultType :: T.Text
  , resultValue :: Value
  } deriving (Generic, Show)

instance ToJSON PatternResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

data SuccessResponse = SuccessResponse
  { successMeta :: Meta
  , successResult :: PatternResult
  , successDiagnostics :: Maybe Diagnostics
  } deriving (Generic, Show)

instance ToJSON SuccessResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 7 }

data ErrorLocation = ErrorLocation
  { errorLocationFile :: Maybe T.Text
  , errorLocationLine :: Maybe Int
  , errorLocationColumn :: Maybe Int
  , errorLocationContext :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ErrorLocation where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 14 }

data ErrorResponse = ErrorResponse
  { errorType :: T.Text
  , errorMessage :: T.Text
  , errorLocation :: Maybe ErrorLocation
  , errorSuggestion :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

data ErrorWrapper = ErrorWrapper
  { errorError :: ErrorResponse
  } deriving (Generic, Show)

instance ToJSON ErrorWrapper where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

-- Pattern serialization to JSON
patternToJSON :: Pattern.Pattern Subject.Subject -> T.Text
patternToJSON pat = unsafePerformIO $ do
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" (utcToZonedTime utc now)
  let patternVal = patternToValue pat
  let response = SuccessResponse
        { successMeta = Meta
            { metaVersion = "0.1.0"
            , metaCommand = "parse"
            , metaTimestamp = timestamp
            , metaHash = ""  -- Will compute after encoding
            }
        , successResult = PatternResult
            { resultType = "Pattern"
            , resultValue = patternVal
            }
        , successDiagnostics = Nothing
        }
  let jsonBytes = encodePretty response
  let hash = T.pack $ show $ SHA256.hash $ BSL.toStrict jsonBytes
  let responseWithHash = response { successMeta = (successMeta response) { metaHash = hash } }
  let finalJsonBytes = encodePretty responseWithHash
  return $ TE.decodeUtf8 $ BSL.toStrict finalJsonBytes

patternToValue :: Pattern.Pattern Subject.Subject -> Value
patternToValue (Pattern.Pattern v es) = object
  [ "value" .= subjectToValue v
  , "elements" .= map patternToValue es
  ]

subjectToValue :: Subject.Subject -> Value
subjectToValue (Subject.Subject sym labels props) = object
  [ "symbol" .= symbolToValue sym
  , "labels" .= toJSON (Set.toList labels)
  , "properties" .= propsToValue props
  ]

symbolToValue :: Subject.Symbol -> Value
symbolToValue (Subject.Symbol s) = toJSON s

propsToValue :: Subject.PropertyRecord -> Value
propsToValue props = object $ map (\(k, v) -> fromString k .= valueToJSON v) (Map.toList props)

valueToJSON :: SubjectValue.Value -> Value
valueToJSON (SubjectValue.VInteger i) = toJSON i
valueToJSON (SubjectValue.VDecimal d) = toJSON d
valueToJSON (SubjectValue.VBoolean b) = toJSON b
valueToJSON (SubjectValue.VString s) = toJSON s
valueToJSON (SubjectValue.VSymbol s) = object ["type" .= ("symbol" :: T.Text), "value" .= s]
valueToJSON (SubjectValue.VTaggedString tag content) = object ["type" .= ("tagged" :: T.Text), "tag" .= tag, "content" .= content]
valueToJSON (SubjectValue.VArray vs) = toJSON (map valueToJSON vs)
valueToJSON (SubjectValue.VMap m) = toJSON (Map.map valueToJSON m)
valueToJSON (SubjectValue.VRange rv) = rangeValueToJSON rv
valueToJSON (SubjectValue.VMeasurement unit val) = object ["type" .= ("measurement" :: T.Text), "unit" .= unit, "value" .= val]

rangeValueToJSON :: SubjectValue.RangeValue -> Value
rangeValueToJSON (SubjectValue.RangeValue lower upper) = object
  [ "type" .= ("range" :: T.Text)
  , "lower" .= toJSON lower
  , "upper" .= toJSON upper
  ]

errorToJSON :: String -> T.Text
errorToJSON msg = 
  let wrapper = ErrorWrapper
        { errorError = ErrorResponse
            { errorType = "ParseError"
            , errorMessage = T.pack msg
            , errorLocation = Nothing
            , errorSuggestion = Nothing
            }
        }
      jsonBytes = encodePretty wrapper
  in TE.decodeUtf8 $ BSL.toStrict jsonBytes

