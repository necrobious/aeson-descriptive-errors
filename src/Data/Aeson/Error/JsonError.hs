module Data.Aeson.Error.JsonError where

import Data.Aeson.Types (Value, ToJSON, toJSON, (.=), object )
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text      as StrictText


-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
-- Descriptive Error Messages
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

-- Brief explanation of the failure, should be presentable to end users in a UI
type Summary = LazyText.Text

-- A Developer-level explanation of the failure, and how to correct it.
type Detail  = LazyText.Text

-- A JSON payload for adding any detail that requires structure.
type AdditionalInfo = Maybe Value 

-- The data type for descriptive error messages. 
data JsonError = JsonError Summary Detail AdditionalInfo 

-- Aeson Value for our descriptive error messages 
instance ToJSON JsonError where
  toJSON (JsonError summary detail additionalInfo) =
    let pairs = [ (StrictText.pack "summary") .= summary
                , (StrictText.pack "detail")  .= detail
                ] 
    in object $ pairs ++ (maybe [] (\ ai -> [(StrictText.pack "additional_info") .= ai]) additionalInfo) 

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
jsonError summary detail additionalInfo = 
  JsonError (LazyText.fromChunks [summary]) (LazyText.fromChunks [detail]) additionalInfo 
jsonErrorString summary detail additionalInfo =
  JsonError (LazyText.pack summary) (LazyText.pack detail) additionalInfo 
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

