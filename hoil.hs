#!/usr/bin/env stack
-- stack runghc --resolver lts-7.13 --install-ghc --package shelly --package string-conversions --package optparse-applicative --package aeson

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, DeriveGeneric, DeriveAnyClass #-}

-- cmd line argument parsing
import Options.Applicative (
  Parser, long, short, switch, help, progDesc,
  fullDesc, helper, info, header, execParser, (<>) )
-- json processing
import Data.Aeson ( (.:), decode, eitherDecode, FromJSON(..), Value(..) )
import GHC.Generics
-- interaction with the shell
import Shelly (shelly, run, cmd, silently)
-- infix version of fmap
import Control.Applicative ( (<$>), (<*>) )
-- string handling
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.String.Conversions (cs)

default (Text)

main :: IO()
main = fmap getModeFrom arguments >>= runMode

data Mode = Version | NonUniqueMode | Open | Tag | Title | Add
  deriving Eq

getModeFrom :: Arguments -> Mode
getModeFrom arguments
  | version arguments = Version
  | multipleModes arguments = NonUniqueMode
  | openMode arguments = Open
  -- default mode is open
  | noMode arguments = Open
  | tagMode arguments = Tag
  | titleMode arguments = Title
  | addMode arguments = Add

multipleModes arguments = numberOfSpecifiedModes arguments > 1
noMode arguments = numberOfSpecifiedModes arguments == 0
arguments = execParser programDescription :: IO Arguments

numberOfSpecifiedModes arguments = sum $ Prelude.map boolToInt modes where
  modes = listOfModesFrom arguments

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

listOfModesFrom :: Arguments -> [Bool]
listOfModesFrom arguments =
  [openMode arguments, tagMode arguments, titleMode arguments, addMode arguments]

runMode :: Mode -> IO()
runMode mode
  | mode == Version = putStrLn currentVersion
  | mode == NonUniqueMode = putStrLn errorMultipleModes
  | mode == Open = decodedBookmarks >>= print
  | otherwise = putStrLn "derp me a herp"

data Bookmark = Bookmark
  { description :: String
  , index :: Int
  , tags :: String
  , title :: String
  , uri :: String } deriving (Show, Generic, FromJSON)

decodedBookmarks = fmap eitherDecode bookmarks :: IO(Either String Bookmark)
bookmarks = fmap cs queryBuku :: IO ByteString

queryBuku :: IO Text
queryBuku = shelly $ silently $ run "buku" ["--print", "1", "--json"]

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

currentVersion = "v0.1.0" :: String
errorMultipleModes =
  "hoil error: more than one of the modes to open/tag/title/add was specified"

-- try to beat 235 lines
