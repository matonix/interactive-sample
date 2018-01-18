{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Default
import Data.Maybe
import Data.Reflection
import Data.Yaml
import Options.Generic
import System.IO

newtype Option = Option
  { optionA :: Maybe String
  } deriving (Generic)

instance ParseRecord Option

newtype Config = Config
  { configB :: Maybe String
  } deriving (Generic)

instance FromJSON Config
instance Default Config

data Settings = Settings
  { settingsA :: String
  , settingsB :: String
  , settingsC :: String
  , settingsD :: String
  } deriving (Show)

main' :: IO ()
main' = do
  settings <- loadSettings
  give settings action

action :: Given Settings => IO ()
action = print (given :: Settings)

loadSettings :: IO Settings
loadSettings = do
  option <- getRecord "Test"
  config <- fromMaybe def <$> decodeFile "config/config.yaml"
  constructSettings option config

constructSettings :: Option -> Config -> IO Settings
constructSettings option config = Settings
  <$> interactiveMaybe "put Option" (optionA option)
  <*> interactiveMaybe "put Config" (configB config)
  <*> interactiveMulti "put multi line text and then Ctrl+D"
  <*> interactive "put one line text"

interactiveMaybe :: String -> Maybe String -> IO String
interactiveMaybe message Nothing = interactive message
interactiveMaybe _ (Just a) = return a

interactive :: String -> IO String
interactive message = putStrLn message >> getLine

interactiveMulti :: String -> IO String
interactiveMulti message = putStrLn message >> getLines []
  where
    -- http://www.mikunimaru.com/entry/2017/11/11/174011
    getLines :: [String] -> IO String
    getLines xs = do
      x <- getLine
      e <- isEOF
      if e
        then return . unlines $ reverse (x:xs)
        else getLines (x:xs)
