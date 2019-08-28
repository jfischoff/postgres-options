{-| A resuable optparse-applicative parser for creating a postgresql-simple
   'Connection'
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.PostgreSQL.Simple.Options
  ( Options(..)
  , defaultOptions
  , toArgs
  , toConnectionString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (Maybe, maybeToList)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

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

toConnectionString :: Options -> ByteString
toConnectionString Options {..} = BSC.pack $ unwords $ map (\(k, v) -> k <> "=" <> v)
  $  maybeToPairStr "host" oHost
  <> maybeToPairStr "hostaddr" oHostaddr
  <> [ ("dbname", oDbname)
     ]
  <> maybeToPair "port" oPort
  <> maybeToPairStr "password" oPassword
  <> maybeToPairStr "user" oUser
  <> maybeToPair "connect_timeout" oConnectTimeout
  <> maybeToPairStr "client_encoding" oClientEncoding
  <> maybeToPairStr "options" oOptions
  <> maybeToPairStr "fallback_applicationName" oFallbackApplicationName
  <> maybeToPair "keepalives" oKeepalives
  <> maybeToPair "keepalives_idle" oKeepalivesIdle
  <> maybeToPair "keepalives_count" oKeepalivesCount
  <> maybeToPairStr "sslmode" oSslmode
  <> maybeToPair "requiressl" oRequiressl
  <> maybeToPair "sslcompression" oSslcompression
  <> maybeToPairStr "sslcert" oSslcert
  <> maybeToPairStr "sslkey" oSslkey
  <> maybeToPairStr "sslrootcert" oSslrootcert
  <> maybeToPairStr "requirepeer" oRequirepeer
  <> maybeToPairStr "krbsrvname" oKrbsrvname
  <> maybeToPairStr "gsslib" oGsslib
  <> maybeToPairStr "service" oService
  where
  maybeToPairStr :: String -> Maybe String -> [(String, String)]
  maybeToPairStr k mv = (k,) <$> maybeToList mv

  maybeToPair :: Show a => String -> Maybe a -> [(String, String)]
  maybeToPair k mv = (\v -> (k, show v)) <$> maybeToList mv

defaultOptions :: String -> Options
defaultOptions dbName = Options {
    oHost                    = Nothing
  , oHostaddr                = Nothing
  , oPort                    = Nothing
  , oUser                    = Nothing
  , oPassword                = Nothing
  , oDbname                  = dbName
  , oConnectTimeout          = Nothing
  , oClientEncoding          = Nothing
  , oOptions                 = Nothing
  , oFallbackApplicationName = Nothing
  , oKeepalives              = Nothing
  , oKeepalivesIdle          = Nothing
  , oKeepalivesCount         = Nothing
  , oSslmode                 = Nothing
  , oRequiressl              = Nothing
  , oSslcompression          = Nothing
  , oSslcert                 = Nothing
  , oSslkey                  = Nothing
  , oSslrootcert             = Nothing
  , oRequirepeer             = Nothing
  , oKrbsrvname              = Nothing
  , oGsslib                  = Nothing
  , oService                 = Nothing
}
