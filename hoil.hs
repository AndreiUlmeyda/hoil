#!/usr/bin/env stack
-- stack runghc --resolver lts-7.13 --install-ghc --package shelly --package string-conversions --package optparse-applicative

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric, DeriveAnyClass #-}

-- cmd line argument parsing
import Options.Applicative (
  Parser, long, short, switch, help, progDesc,
  fullDesc, helper, info, header, execParser, (<>) )
-- interaction with the shell
import Shelly (shelly, run, silently)
-- json processing
import Data.Aeson ( (.:), decode, FromJSON(..), Value(..) )
import GHC.Generics
-- infix version of fmap
import Control.Applicative ( (<$>), (<*>) )
-- string handling
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)

default (Text)

main :: IO()
main = do
  arguments <- execParser programDescription
  bookmarkJson <- fmap cs getBookmark :: IO ByteString
  let (Just bookmark) = decode bookmarkJson :: Maybe Bookmark
  print bookmark

queryBuku :: IO Text
queryBuku = shelly $ silently $ run "buku" ["--print", "1", "--json"]

-- the data structure to parse bookmark data into
data Bookmark = Bookmark
  { description :: String
  , index :: Int
  , tags :: String
  , title :: String
  , uri :: String } deriving (Show, Generic, FromJSON)

programDescription = info (helper <*> argumentParser)
  ( fullDesc
  <> progDesc "Open or manipulate buku managed bookmarks."
  <> header "hoil - command line continuous search frontend for the buku bookmark manager" )

data Arguments = Arguments
  { version :: Bool
  , openMode :: Bool
  , tagMode :: Bool
  , titleMode :: Bool
  , addMode :: Bool
  , pecoConfiguration :: Bool }
  deriving (Show)

argumentParser :: Parser Arguments
argumentParser = Arguments
  <$> switch
    ( long "version"
    <> short 'v'
    <> help "Show version information" )
  <*> switch
    ( long "open"
    <> short 'o'
    <> help "Open selected bookmarks in the browser" )
  <*> switch
    ( long "tag"
    <> short 't'
    <> help "Apply tags to selected bookmarks" )
  <*> switch
    ( long "title"
    <> short 'T'
    <> help "Set new titles for selected bookmarks" )
  <*> switch
    ( long "add"
    <> short 'a'
    <> help "Add a bookmark from clipboard" )
  <*> switch
    ( long "no-peco-reconfiguration"
    <> short 'p'
    <> help "Do not overwrite existing peco configuration" )

