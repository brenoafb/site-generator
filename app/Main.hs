{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Utils
import Views
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" indexView
  get "/posts" postsIndexView
  get "/posts/:post" $ do
    postName <- param "post"
    postView postName
