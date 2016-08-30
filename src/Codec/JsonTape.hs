{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Codec.JsonTape
  ( Object
  , decodeFiles
  , encodeFiles)
  where

import Data.Aeson
       (FromJSON, ToJSON, (.=), (.:), eitherDecodeStrict, encode, object,
        parseJSON, toJSON, withObject)
import Data.ByteString.Base64.Type (ByteString, ByteString64)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Directory.Tree (DirTree(..))

-- | Type alias representing a directory tree packable into json-tape.
type Object = DirTree ByteString64

-- | Aeson deserialisation of a directory tree.
instance FromJSON Object where
    parseJSON =
        withObject "file or directory" $
        \o -> do
            kind <- o .: "kind"
            case kind of
                "file" -> do
                    File <$> (o .: "name") <*> (o .: "content")
                "directory" -> Dir <$> (o .: "name") <*> (o .: "content")
                _ -> fail ("unknown kind: " <> kind)

-- | Aeson serialisation of a directory tree.
instance ToJSON Object where
    toJSON o =
        case o of
            File {name = name
                 ,file = file} ->
                object
                    [ "name" .= name
                    , "kind" .= ("file" :: Text)
                    , "content" .= file]
            Dir {name = name
                ,contents = contents} ->
                object
                    [ "name" .= name
                    , "kind" .= ("directory" :: Text)
                    , "content" .= contents]
            Failed {name = name} ->
                object ["name" .= name, "kind" .= ("failed" :: Text)]

-- | Serialise @Object@ into json-tape.
encodeFiles :: Object -> ByteString
encodeFiles = toStrict . encode

-- | Deserialise json-tape into @Object@ representation.
decodeFiles :: ByteString -> Either String Object
decodeFiles = eitherDecodeStrict
