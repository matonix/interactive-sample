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
  <$> interactiveMaybe "put Option" id (optionA option)
  <*> interactiveMaybe "put Config" id (configB config)
  <*> interactiveMulti "put multi line text and then Ctrl+D" unlines
  <*> interactive "put one line text" id

interactiveMaybe :: String -> (String -> a) -> Maybe String -> IO a
interactiveMaybe message f Nothing = interactive message f
interactiveMaybe _ f (Just a) = return $ f a

interactive :: String -> (String -> a) -> IO a
interactive message f = f <$ putStrLn message <*> getLine

interactiveMulti :: String -> ([String] -> a) -> IO a
interactiveMulti message f = f <$ putStrLn message <*> getLines []
  where
    -- http://www.mikunimaru.com/entry/2017/11/11/174011
    getLines :: [String] -> IO [String]
    getLines xs = do
      x <- getLine
      e <- isEOF
      if e
        then return $ reverse (x:xs)
        else getLines (x:xs)
