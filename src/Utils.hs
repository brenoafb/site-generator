{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Commonmark
import Data.Aeson()
import Network.HTTP.Types.Status
import Control.Monad.Except
import Web.Scotty.Internal.Types (ActionError(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

readFile' :: T.Text -> IO T.Text
readFile' = TIO.readFile . T.unpack

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

parseMarkdown :: (MonadIO m, MonadError (ActionError T.Text) m)
              => T.Text -> m (Html ())
parseMarkdown t = do
  x <- liftIO (parseCommonmarkWith defaultSyntaxSpec
               (tokenize "source" $ T.toStrict t) :: IO (Either ParseError (Html ())))
  case x of
    Right md -> pure md
    Left e  -> throwError . mkActionError . T.pack $ show e

mkActionError :: e -> ActionError e
mkActionError e = ActionError status404 e
