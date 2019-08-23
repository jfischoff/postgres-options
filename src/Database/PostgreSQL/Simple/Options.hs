{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.PostgreSQL.Simple.Options where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Maybe (Maybe, maybeToList)

data Options = Options
  { oHost                    :: Maybe String
  , oHostaddr                :: Maybe String
  , oPort                    :: Maybe Int
  , oUser                    :: Maybe String
  , oPassword                :: Maybe String
  , oDbname                  :: String
  , oConnectTimeout          :: Maybe Int
  , oClientEncoding          :: Maybe String
  , oOptions                 :: Maybe String
  , oFallbackApplicationName :: Maybe String
  , oKeepalives              :: Maybe Int
  , oKeepalivesIdle          :: Maybe Int
  , oKeepalivesCount         :: Maybe Int
  , oSslmode                 :: Maybe String
  , oRequiressl              :: Maybe Int
  , oSslcompression          :: Maybe Int
  , oSslcert                 :: Maybe String
  , oSslkey                  :: Maybe String
  , oSslrootcert             :: Maybe String
  , oRequirepeer             :: Maybe String
  , oKrbsrvname              :: Maybe String
  , oGsslib                  :: Maybe String
  , oService                 :: Maybe String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

toArgs :: Options -> [String]
toArgs Options {..} =
  [ "--dbname=" <> oDbname
  ]
  ++ (("--host=" <>) <$> maybeToList oHost)
  ++ (("--username=" <>) <$> maybeToList oUser)
  ++ (("--password=" <>) <$> maybeToList oPassword)
  ++ ((\x -> "--host=" <> show x) <$> maybeToList oPort)

