{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Uuid(textUuid)
import Web.Scotty

import Data.Monoid (mconcat)

webserver = scotty 3000 $ do
    get "/" $ do
        html $ mconcat ["<h1> My First webpage using Haskell Scotty!</h1>"]
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]