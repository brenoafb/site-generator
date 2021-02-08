{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Commonmark
import Data.Aeson()
import Control.Monad.Except
import System.Directory
import GHC.Generics
import Web.Scotty
import qualified Data.Text.Lazy as T

main :: IO ()
main = scotty 3000 $ do
  get "/" indexView
  get "/posts" postsIndexView
  get "/posts/:post" $ do
    postName <- param "post"
    postView postName

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

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

postView :: T.Text -> ActionM ()
postView postName = do
  contents <- safeRead $ "pages/posts/" <> postName <> ".md"
  parsed   <- parseMarkdown contents
  html $ renderHtml parsed
  `catchError` (\_ -> html "ERROR")

indexView :: ActionM ()
indexView = do
  contents <- liftIO $ T.pack <$> readFile "pages/index.md"
  mdE <- liftIO (parseCommonmarkWith defaultSyntaxSpec (tokenize "source" $ T.toStrict contents) :: IO (Either ParseError (Html ())))
  case mdE of
    Left e  -> error "Error parsing markdown"
    Right h -> html $ renderHtml h

safeRead :: (MonadIO m, MonadError e m) -- (Exception e, IsString e)
         => T.Text -> m T.Text
safeRead path = liftIO $ T.pack <$> readFile (T.unpack path)
-- TODO handle exceptions

parseMarkdown :: (MonadIO m, MonadError e m)
              => T.Text -> m (Html ())
parseMarkdown t = do
  x <- liftIO (parseCommonmarkWith defaultSyntaxSpec (tokenize "source" $ T.toStrict t) :: IO (Either ParseError (Html ())))
  case x of
    Right x -> pure x
    Left e  -> throwError undefined -- TODO actually throw an error message
    -- Left e  -> throwError $ show e
