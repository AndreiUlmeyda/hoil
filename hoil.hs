#!/usr/bin/env stack
-- stack runghc --resolver lts-7.13 --install-ghc --package shelly --package string-conversions

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Shelly
import Data.Text as T

import Data.Aeson ((.:), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)
default (T.Text)

main :: IO()
main = do
  bookmarkJson <- fmap cs getBookmark :: IO ByteString
  case decodeBookmark bookmarkJson of
    Just bookmark -> print bookmark
    Nothing -> print "No bookmark could be parsed."

getBookmark :: IO Text
getBookmark = shelly $ run buku printAllAsJson
  where
    buku = "buku"
    -- arguments to buku go into a list of strings
    printAllAsJson = ["--print", "1", "--json"]

decodeBookmark :: ByteString -> Maybe Bookmark
decodeBookmark = decode

-- the data structure to parse bookmark data into
data Bookmark = Bookmark
  { description :: String
  , index :: Int
  , tags :: String
  , title :: String
  , uri :: String } deriving (Show)

-- make it an instance of the type expected by the parsing library
instance FromJSON Bookmark where
  parseJSON (Object o) = Bookmark
    <$> (o .: "description")
    <*> (o .: "index")
    <*> (o .: "tags")
    <*> (o .: "title")
    <*> (o .: "uri")
