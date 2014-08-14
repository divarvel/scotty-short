{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shortner.AuthManagement where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Char8 (unpack, split, pack)
import           Data.Functor ((<$>))
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT

import           Network.HTTP.Types.Status (created201, status204, status401, status403, status404)

import           System.Environment (getEnv)

import           Web.Scotty

credentials = do
    user <- pack <$> getEnv "ADMIN_USER"
    pwd <- pack <$> getEnv "ADMIN_PASSWORD"
    return (user, pwd)

basicAuth :: ActionM (Maybe (BS.ByteString, BS.ByteString))
basicAuth = do
    authHeader :: Maybe LT.Text <- header "Authorization"
    return $ parseBasicAuth =<< (TE.encodeUtf8 .  LT.toStrict) <$> authHeader

parseBasicAuth :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseBasicAuth h = let
    (authScheme, encoded) = BS.splitAt 6 h
    in
        if authScheme == "Basic " then
            let
                decoded = B64.decode encoded
                components = fmap (split ':') decoded
                tup c = case c of
                    (x:y:_) -> Just (x, y)
                    _ -> Nothing
            in either (const Nothing) Just components >>= tup
        else
            Nothing

ensureBasicAuth action = do
    adminCreds <- liftIO credentials
    givenCreds <- basicAuth
    case givenCreds of
        (Just cs) -> if cs == adminCreds then action else status status403
        Nothing -> addHeader "WWW-Authenticate" "Basic realm=\"admin\"" >> status status401

