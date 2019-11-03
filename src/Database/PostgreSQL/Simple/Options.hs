{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.PostgreSQL.Simple.Options
  ( Options(..)
  , defaultOptions
  , toArgs
  , toConnectionString
  , completeOptions
  , PartialOptions (..)
  , parseConnectionString
  , defaultPartialOptions
  ) where

import Data.Maybe (Maybe, maybeToList)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Either.Validation as DEV
import Data.Either.Validation (Validation(..), validationToEither)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import URI.ByteString as URI
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import Generics.Deriving.Monoid (gmappenddefault, gmemptydefault)
import Control.Monad ((<=<), foldM)
import Control.Applicative

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

-- | A monodial version of 'Options'.
--   Useful for combining many options from different sources
data PartialOptions = PartialOptions
  { host                    :: Last String
  , hostaddr                :: Last String
  , port                    :: Last Int
  , user                    :: Last String
  , password                :: Last String
  , dbname                  :: Last String
  , connectTimeout          :: Last Int
  , clientEncoding          :: Last String
  , options                 :: Last String
  , fallbackApplicationName :: Last String
  , keepalives              :: Last Int
  , keepalivesIdle          :: Last Int
  , keepalivesCount         :: Last Int
  , sslmode                 :: Last String
  , requiressl              :: Last Int
  , sslcompression          :: Last Int
  , sslcert                 :: Last String
  , sslkey                  :: Last String
  , sslrootcert             :: Last String
  , requirepeer             :: Last String
  , krbsrvname              :: Last String
  , gsslib                  :: Last String
  , service                 :: Last String
  } deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance Semigroup PartialOptions where
  (<>) = gmappenddefault

instance Monoid PartialOptions where
  mempty = gmemptydefault

defaultPartialOptions :: PartialOptions
defaultPartialOptions = mempty
  { host     = pure "localhost"
  , port     = pure 5432
  , user     = pure "postgres"
  , dbname   = pure "postgres"
  }

getLast' :: Applicative f => Last a -> f (Maybe a)
getLast' = pure . getLast

completeOptions :: PartialOptions -> Either [String] Options
completeOptions PartialOptions {..} = validationToEither $
  Options <$> getLast' host
          <*> getLast' hostaddr
          <*> (fmap fromIntegral <$> getLast' port)
          <*> getLast' user
          <*> getLast' password
          <*> getOption "dbname" dbname
          <*> getLast' connectTimeout
          <*> getLast' clientEncoding
          <*> getLast' options
          <*> getLast' fallbackApplicationName
          <*> getLast' keepalives
          <*> getLast' keepalivesIdle
          <*> getLast' keepalivesCount
          <*> getLast' sslmode
          <*> getLast' requiressl
          <*> getLast' sslcompression
          <*> getLast' sslcert
          <*> getLast' sslkey
          <*> getLast' sslrootcert
          <*> getLast' requirepeer
          <*> getLast' krbsrvname
          <*> getLast' gsslib
          <*> getLast' service

userInfoToPartialOptions :: UserInfo -> PartialOptions
userInfoToPartialOptions UserInfo {..} = mempty { user = return $ BSC.unpack uiUsername } <> if BS.null uiPassword
  then mempty
  else mempty { password = return $ BSC.unpack uiPassword }

autorityToPartialOptions :: Authority -> PartialOptions
autorityToPartialOptions Authority {..} = maybe mempty userInfoToPartialOptions authorityUserInfo <>
  mempty { host = return $ BSC.unpack $ hostBS authorityHost } <>
  maybe mempty (\p -> mempty { port = return $ portNumber p }) authorityPort

pathToPartialOptions :: ByteString -> PartialOptions
pathToPartialOptions path = case drop 1 $ BSC.unpack path of
  "" -> mempty
  x  -> mempty {dbname = return x }

parseInt :: String -> String -> Either String Int
parseInt msg v = maybe (Left (msg <> " value of: " <> v <> " is not a number")) Right $
      readMaybe v

getOption :: String -> Last a -> Validation [String] a
getOption optionName = \case
    Last (Just x) -> pure x
    Last Nothing  -> DEV.Failure ["Missing " ++ optionName ++ " option"]

parseString :: String -> Maybe String
parseString x = readMaybe x <|> unSingleQuote x <|> Just x

unSingleQuote :: String -> Maybe String
unSingleQuote (x : xs@(_ : _))
  | x == '\'' && last xs == '\'' = Just $ init xs
  | otherwise                    = Nothing
unSingleQuote _                  = Nothing

keywordToPartialOptions :: String -> String -> Either String PartialOptions
keywordToPartialOptions k v = case k of
  "host" -> return $ mempty { host = return v }
  "hostaddress" -> return $ mempty { hostaddr = return v }
  "port" -> do
    portValue <- parseInt "port" v
    return $ mempty { port = return portValue }
  "user" -> return $ mempty { user = return v }
  "password" -> return $ mempty { password = return v }
  "dbname" -> return $ mempty { dbname = return v}
  "connect_timeout" -> do
    x <- parseInt "connect_timeout" v
    return $ mempty { connectTimeout = return x }
  "client_encoding" -> return $ mempty { clientEncoding = return v }
  "options" -> return $ mempty { options = return v }
  "fallback_applicationName" -> return $ mempty { fallbackApplicationName = return v }
  "keepalives" -> do
    x <- parseInt "keepalives" v
    return $ mempty { keepalives = return x }
  "keepalives_idle" -> do
    x <- parseInt "keepalives_idle" v
    return $ mempty { keepalivesIdle = return x }
  "keepalives_count" -> do
    x <- parseInt "keepalives_count" v
    return $ mempty { keepalivesCount = return x }
  "sslmode" -> return $ mempty { sslmode = return v }
  "requiressl" -> do
    x <- parseInt "requiressl" v
    return $ mempty { requiressl = return x }
  "sslcompression" -> do
    x <- parseInt "sslcompression" v
    return $ mempty { sslcompression = return x }
  "sslcert" -> return $ mempty { sslcert = return v }
  "sslkey" -> return $ mempty { sslkey = return v }
  "sslrootcert" -> return $ mempty { sslrootcert = return v }
  "requirepeer" -> return $ mempty { requirepeer = return v }
  "krbsrvname" -> return $ mempty { krbsrvname = return v }
  "gsslib" -> return $ mempty { gsslib = return v }
  "service" -> return $ mempty { service = return v }

  x -> Left $ "Unrecongnized option: " ++ show x

queryToPartialOptions :: URI.Query -> Either String PartialOptions
queryToPartialOptions Query {..} = foldM (\acc (k, v) -> fmap (mappend acc) $ keywordToPartialOptions (BSC.unpack k) $ BSC.unpack v) mempty queryPairs

uriToOptions :: URIRef Absolute -> Either String PartialOptions
uriToOptions URI {..} = case schemeBS uriScheme of
  "postgresql" -> do
    queryParts <- queryToPartialOptions uriQuery
    return $ maybe mempty autorityToPartialOptions uriAuthority <>
      pathToPartialOptions uriPath <> queryParts

  x -> Left $ "Wrong protocol. Expected \"postgresql\" but got: " ++ show x

parseURIStr :: String -> Either String (URIRef Absolute)
parseURIStr = left show . parseURI strictURIParserOptions . BSC.pack where
  left f = \case
    Left x -> Left $ f x
    Right x -> Right x

parseKeywords :: String -> Either String PartialOptions
parseKeywords [] = Left "Failed to parse keywords"
parseKeywords x = fmap mconcat . mapM (uncurry keywordToPartialOptions <=< toTuple . splitOn "=") $ words x where
  toTuple [k, v] = return (k, v)
  toTuple xs = Left $ "invalid opts:" ++ show (intercalate "=" xs)

parseConnectionString :: String -> Either String PartialOptions
parseConnectionString url = do
  url' <- maybe (Left "failed to parse as string") Right $ parseString url
  parseKeywords url' <|> (uriToOptions =<< parseURIStr url')