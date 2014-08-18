{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Web.Scotty

import           Data.Aeson hiding (json)
import           Data.Aeson.TH
import           Control.Applicative ((<*>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Char8 (unpack, split, pack)
import           Data.Functor ((<$>))
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Monoid ((<>))
import           Data.Pool (createPool, withResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import           Data.UUID
import           Data.UUID.Aeson
import           Data.UUID.V4
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Network.HTTP.Types.Status (created201, status204, status401, status403, status404)
import           Network.Wai (requestHeaderHost, requestMethod, responseLBS)
import           System.Environment (getEnv)


import Shortner.AuthManagement
import Shortner.Database
import Shortner.HtmlPages
import Shortner.Types

mkLink :: Domain -> T.Text -> T.Text -> IO Link
mkLink (Domain domain_id _ _) code long_url = do
    id <- nextRandom
    return $ Link id domain_id code long_url 0


main = scotty 8080 $ do
  post "/urls" $ ensureBasicAuth $ do
    code :: T.Text <- param "code"
    long_url :: T.Text <- param "long_url"
    hostname <- (fromMaybe "" . requestHeaderHost) <$> request
    mdomain <- liftIO $ getDomainByHostname hostname
    case mdomain of
        (Just domain) -> do
            link <- liftIO $ mkLink domain code long_url
            liftIO $ insertLink link
            status created201
            json link
        Nothing -> status status404

  get "/urls" $ ensureBasicAuth $ do
    links <- liftIO getAllLinks
    json links

  get "/urls/:id" $ ensureBasicAuth $ do
    id <- param "id"
    mlink <- liftIO $ getLinkById id
    case mlink of
        (Just link) -> json link
        Nothing -> status status404

  delete "/urls/:id" $ ensureBasicAuth $ do
    id <- param "id"
    liftIO $ removeLink id
    status status204

  get "/:shorturl" $ do
    shorturl :: T.Text <- param "shorturl"
    hostname <- (fromMaybe "" . requestHeaderHost) <$> request
    mlink <- liftIO $ getLinkByCode hostname shorturl
    case mlink of
        (Just link) -> do
            liftIO . updateLinkHits . linkId $ link
            redirect . LT.fromStrict . longUrl $ link
        Nothing -> do
            status status404
            html codeNotFoundPage


