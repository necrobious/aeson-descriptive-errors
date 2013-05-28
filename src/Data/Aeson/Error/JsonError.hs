{-# LANGUAGE Rank2Types, ExistentialQuantification #-}
module Data.Aeson.Error.JsonError where

import Data.Aeson.Types (Value, ToJSON, toJSON, (.=), object )
import qualified Data.Text                  as StrictText
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Encoding         as StrictTextEncoding
import qualified Data.Text.Lazy.Encoding    as LazyTextEncoding
import qualified Data.ByteString            as StrictByteString
import qualified Data.ByteString.Char8      as StrictChar8 
import qualified Data.ByteString.Lazy       as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import Data.Textual  

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
-- Descriptive Error Messages
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

{- we want error messages to be consistent across an application.
   we define a JsonError datatype as an envelope for three params:
     * Brief explanation of the failure, should be presentable to end users in a UI
     * A Developer-level explanation of the failure, and how to correct it.
     * A JSON payload for adding any additional information.
-}
data JsonError = forall summary detail. (Textual summary, Textual detail) => JsonError summary detail (Maybe Value) 

-- Aeson Value for our descriptive error messages 
instance ToJSON JsonError where
  toJSON (JsonError summary detail additionalInfo) =
    let pairs = [ (toText "summary") .= (toText summary)
                , (toText "detail")  .= (toText detail)
                ] 
    in object $ pairs ++ (maybe [] (\ ai -> [(toText "additional_info") .= ai]) additionalInfo) 
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

