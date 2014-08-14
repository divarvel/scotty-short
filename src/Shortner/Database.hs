{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Shortner.Database where


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
import           System.Environment (getEnv)
import Web.Scotty

import Shortner.Types

pgConn = connectPostgreSQL =<< pack <$> getEnv "CONNSTRING"
pgPool = createPool pgConn close 1 10 10
withPgConnection action = do
    pool <- pgPool
    withResource pool action

getLinkByCodeQuery =
    "select url_id, domain_id, code, long_url, hits " <>
     "from url " <>
     "inner join domain using (domain_id) " <>
     "where url.code=? and domain.hostname=? limit 1"

getLinkByIdQuery = "select url_id, domain_id, code, long_url, hits from url where url_id = ? limit 1"
updateLinkHitsQuery = "update url set hits = hits + 1 where url_id = ?"
createLinkQuery = "insert into url (url_id, domain_id, code, long_url, hits) values (?, ?, ?, ?, ?)"
removeLinkQuery = "delete from url where url_id = ?"

getDomainByHostnameQuery = "select domain_id, hostname, name from domain  where hostname = ? limit 1"

getAllLinks :: IO [Link] = withPgConnection $ \conn ->
    query_ conn "select url_id, domain_id, code, long_url, hits from url"

getLinkByCode :: BS.ByteString -> T.Text -> IO (Maybe Link)
getLinkByCode domain code = withPgConnection $ \conn ->
    listToMaybe <$> query conn getLinkByCodeQuery (code, domain)

getLinkById :: UUID -> IO (Maybe Link)
getLinkById link_id = withPgConnection $ \conn ->
    listToMaybe <$> query conn getLinkByIdQuery (Only link_id)

updateLinkHits :: UUID -> IO ()
updateLinkHits link_id = withPgConnection $ \conn ->
    const () <$> execute conn updateLinkHitsQuery (Only link_id)

insertLink :: Link -> IO ()
insertLink (Link l_id d_id code long_url hits) = withPgConnection $ \conn ->
    const () <$> execute conn createLinkQuery (l_id, d_id, code, long_url, hits)

getDomainByHostname :: BS.ByteString -> IO (Maybe Domain)
getDomainByHostname hostname = withPgConnection $ \conn ->
    listToMaybe <$> query conn getDomainByHostnameQuery (Only hostname)

removeLink :: UUID -> IO ()
removeLink l_id = withPgConnection $ \conn ->
    const () <$> execute conn removeLinkQuery (Only l_id)



