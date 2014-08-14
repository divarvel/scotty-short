{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Shortner.Types where

import           Control.Applicative ((<*>))
import           Data.Aeson hiding (json)
import           Data.Aeson.TH
import           Data.Functor ((<$>))
import           Data.UUID
import           Data.UUID.Aeson
import           Data.UUID.V4
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Web.Scotty


data Domain = Domain
    { domainId :: UUID
    , hostname :: T.Text
    , name :: T.Text
    }

instance FromRow Domain where
  fromRow = Domain <$> field <*> field <*> field

data Link = Link
    { linkId :: UUID
    , linkDomainId :: UUID
    , code :: T.Text
    , longUrl :: T.Text
    , hits :: Integer
    } deriving (Show, Eq)

$(deriveToJSON defaultOptions ''Link)

instance FromRow Link where
  fromRow = Link <$> field <*> field <*> field <*> field <*> field

instance Parsable UUID where
  parseParam p = maybe (Left "") Right $ fromString (LT.unpack p)
