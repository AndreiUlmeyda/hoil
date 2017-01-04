#!/usr/bin/env stack
-- stack runghc --resolver lts-7.13 --install-ghc --package shelly --package string-conversions --package optparse-applicative

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric, DeriveAnyClass #-}

-- cmd line argument parsing
import Options.Applicative (
  Parser, long, short, switch, help, progDesc,
  fullDesc, helper, info, header, execParser, (<>) )
-- json processing
import Data.Aeson ( (.:), decode, eitherDecode, FromJSON(..), Value(..) )
-- interaction with the shell
import Shelly (shelly, run, silently)
-- json processing
import GHC.Generics
-- infix version of fmap
import Control.Applicative ( (<$>), (<*>) )
-- string handling
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)

default (Text)

main :: IO()
main = process <$> arguments <*> bookmarks >>= print

process :: Arguments -> ByteString -> String
process arguments bookmarks =
  show (eitherDecode bookmarks :: Either String Bookmark) ++
  show (getModeFrom arguments)

arguments :: IO Arguments
arguments = execParser programDescription

bookmarks :: IO ByteString
bookmarks = fmap cs queryBuku

queryBuku :: IO Text
queryBuku = shelly $ silently $ run "buku" ["--print", "1", "--json"]

data Mode = Version | NonUniqueMode | Open | Tag | Title | Add
  deriving Show

getModeFrom :: Arguments -> Mode
getModeFrom arguments
  | version arguments = Version
  | nonUniqueMode arguments = NonUniqueMode
  | openMode arguments = Open
  | tagMode arguments = Tag
  | titleMode arguments = Title
  | addMode arguments = Add

nonUniqueMode :: Arguments -> Bool
nonUniqueMode arguments = numberOfSpecifiedModes /= 1 where
  numberOfSpecifiedModes = sum $ Prelude.map boolToInt modes
  modes = [openMode arguments, tagMode arguments, titleMode arguments, addMode arguments]

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

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


-- help text printing is handled directly by the parsing library, therefore,
-- neither here nor as a mode, does it appear explicitly
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

-- try to beat 235 lines
