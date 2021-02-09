{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Views where

import Utils
import Commonmark
import Data.Aeson()
import Control.Monad.Except
import System.Directory
import Web.Scotty
import qualified Data.Text.Lazy as T

postsIndexView :: ActionM ()
postsIndexView = do
  contents <- (T.pack <$>) <$> liftIO (getDirectoryContents "pages/posts")
  let postsM = sequenceA . filter isJust $ map (T.stripSuffix ".md") contents
  case postsM of
    Nothing -> pure () -- Should never happen
    Just posts ->
      html
      $ mconcat
      $ ["<h1>Posts</h1><ul>"]
      ++ [formatItem p | p <- posts]
      ++ ["</ul>"]
  where formatItem n = "<li><a href=\"/posts/" <> n <> "\">" <> n <> "</a></li>"

postView :: T.Text -> ActionM ()
postView postName = do
  contents <- liftIO . readFile' $ "pages/posts/" <> postName <> ".md"
  parsed   <- parseMarkdown contents
  html $ renderHtml parsed

indexView :: ActionM ()
indexView = do
  contents <- liftIO $ readFile' "pages/index.md"
  md <- parseMarkdown contents
  html $ renderHtml md
