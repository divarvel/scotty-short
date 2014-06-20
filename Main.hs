{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import           Data.UUID
import           Data.UUID.Aeson
import           Data.UUID.V4
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Network.HTTP.Types.Status (created201, status204, status401, status403, status404)
import           Network.Wai (requestHeaderHost)
import           System.Environment (getEnv)

data Domain = Domain
    { domain_id :: UUID
    , hostname :: T.Text
    , name :: T.Text
    }

instance FromRow Domain where
  fromRow = Domain <$> field <*> field <*> field

data Link = Link
    { link_id :: UUID
    , link_domain_id :: UUID
    , code :: T.Text
    , long_url :: T.Text
    , hits :: Integer
    } deriving (Show, Eq)

$(deriveToJSON defaultOptions ''Link)

instance FromRow Link where
  fromRow = Link <$> field <*> field <*> field <*> field <*> field

instance Parsable UUID where
  parseParam p = maybe (Left "") (Right) $ fromString (LT.unpack p)

credentials = do
    user <- pack <$> getEnv "ADMIN_USER"
    pwd <- pack <$> getEnv "ADMIN_PASSWORD"
    return (user, pwd)

pgConn = connectPostgreSQL =<< pack <$> getEnv "CONNSTRING"


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
            in (either (const Nothing) Just components) >>= tup
        else
            Nothing

ensureBasicAuth action = do
    adminCreds <- liftIO $ credentials
    givenCreds <- basicAuth
    case givenCreds of
        (Just adminCreds) -> action
        (Just _) -> status status403
        Nothing -> addHeader "WWW-Authenticate" "Basic realm=\"admin\"" >> status status401

mkLink :: Domain -> T.Text -> T.Text -> IO Link
mkLink (Domain domain_id _ _) code long_url = do
    id <- nextRandom
    return $ Link id domain_id code long_url 0

main = scotty 8080 $ do
  get "/" $ do
    file "static/index.html"

  get "/static/files/resume.pdf" $ do
    file "static/resume.pdf"

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
            liftIO . updateLinkHits . link_id $ link
            redirect . LT.fromStrict . long_url $ link
        Nothing -> status status404


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

getAllLinks :: IO [Link] = do
    conn <- pgConn
    query_ conn "select url_id, domain_id, code, long_url, hits from url"

getLinkByCode :: BS.ByteString -> T.Text -> IO (Maybe Link)
getLinkByCode domain code = do
    conn <- pgConn
    listToMaybe <$> query conn getLinkByCodeQuery (code, domain)

getLinkById :: UUID -> IO (Maybe Link)
getLinkById link_id = do
    conn <- pgConn
    listToMaybe <$> query conn getLinkByIdQuery (Only link_id)

updateLinkHits :: UUID -> IO ()
updateLinkHits link_id = do
    conn <- pgConn
    const () <$> execute conn updateLinkHitsQuery (Only link_id)

insertLink :: Link -> IO ()
insertLink (Link l_id d_id code long_url hits) = do
    conn <- pgConn
    const () <$> execute conn createLinkQuery (l_id, d_id, code, long_url, hits)

getDomainByHostname :: BS.ByteString -> IO (Maybe Domain)
getDomainByHostname hostname = do
    conn <- pgConn
    listToMaybe <$> query conn getDomainByHostnameQuery (Only hostname)

removeLink :: UUID -> IO ()
removeLink l_id = do
    conn <- pgConn
    const () <$> execute conn removeLinkQuery (Only l_id)

