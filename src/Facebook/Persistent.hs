{-# LANGUAGE OverloadedStrings, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This package is meant to be imported as
--
-- > import Facebook.Persistent ()
--
-- because it doesn't export any symbols.  However, it exports
-- the following orphan instances:
--
-- @
-- instance 'PersistField' 'Action'          -- since 0.1
-- instance 'PersistField' 'Id'              -- since 0.1.2
-- instance 'PersistField' 'AppAccessToken'  -- since 0.1.3
-- instance 'PersistField' 'UserAccessToken' -- since 0.1.3
-- @
module Facebook.Persistent () where

import Control.Applicative ((<$>), (<*>))
import Data.String (fromString)
import Data.Text (Text)
import Data.Int (Int64)
import Data.Word (Word8)
import Database.Persist
import Facebook
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock as T
import qualified Data.Time.Clock.POSIX as T

-- | From @fb-persistent@.  Since 0.1.
instance PersistField Action where
    toPersistValue = toPersistValue . show
    fromPersistValue v =
        case fmap reads $ fromPersistValue v of
          Right [(action,"")] -> Right action
          Right _  -> Left "fromPersistValue[Facebook.Action]: \
                           \Could not parse action"
          Left err -> Left err
    sqlType = sqlType . show
    isNullable _ = False


-- | From @fb-persistent@.  Since 0.1.2.
--
-- We use 'ByteString' for historical purposes in order to
-- maintain compatibility with @fb < 0.13@.
instance PersistField Id where
    toPersistValue = toPersistValue . TE.encodeUtf8 . idCode
    fromPersistValue v = Id . TE.decodeUtf8 <$> fromPersistValue v
    sqlType = sqlType . TE.encodeUtf8 . idCode
    isNullable _ = False


-- | From @fb-persistent@.  Since 0.1.3.  Note that your fields
-- should either be of type 'UserAccessToken' or
-- 'AppAccessToken', not a polymorphic @'AccessToken' kind@.
-- Expiration time of 'UserAccessToken'@s@ is saved with
-- precision of one second.
instance AccessTokenKind kind => PersistField (AccessToken kind) where
    toPersistValue = toPersistValue . S.runPut . accessTokenPut
    fromPersistValue v = do
      bs <- fromPersistValue v
      either (Left . fromString) Right (S.runGet accessTokenGet bs)
    sqlType = sqlType . S.runPut . accessTokenPut
    isNullable _ = False


-- | Since 'AccessToken' is a GADT, our 'S.get' function needs a
-- type class in order to be implemented.  Which 'accessTokenGet'
-- gets used is chosen in compile-time.  As a bonus, we implement
-- 'S.put' in the same way.
class AccessTokenKind kind where
  accessTokenPut :: AccessToken kind -> S.Put
  accessTokenGet :: S.Get (AccessToken kind)

instance AccessTokenKind UserKind where
  accessTokenPut (UserAccessToken uid token expires) = do
    S.putWord8 0
    putId uid
    putText token
    putUTCTime expires
  accessTokenGet = do
    v <- S.getWord8
    case v of
      0 -> UserAccessToken <$> getId <*> getText <*> getUTCTime
      1 -> fail $ "fb-persistent: AccessToken: tried to decode an AppAccessToken as UserAccessToken."
      _ -> accessTokenUnknownVersion v

instance AccessTokenKind AppKind where
  accessTokenPut (AppAccessToken token) = do
    S.putWord8 1
    putText token
  accessTokenGet = do
    v <- S.getWord8
    case v of
      1 -> AppAccessToken <$> getText
      0 -> fail $ "fb-persistent: AccessToken: tried to decode an UserAccessToken as AppAccessToken."
      _ -> accessTokenUnknownVersion v

getText :: S.Get Text
getText = TE.decodeUtf8 <$> S.get

putText :: Text -> S.Put
putText = S.put . TE.encodeUtf8

getId :: S.Get Id
getId = Id <$> getText

putId :: Id -> S.Put
putId = putText . idCode


putUTCTime :: T.UTCTime -> S.Put
putUTCTime = S.put . toInt64 . T.utcTimeToPOSIXSeconds
  where
    toInt64 :: T.POSIXTime -> Int64
    toInt64 = truncate

getUTCTime :: S.Get T.UTCTime
getUTCTime = T.posixSecondsToUTCTime . fromInt64 <$> S.get
  where
    fromInt64 :: Int64 -> T.POSIXTime
    fromInt64 = fromIntegral


accessTokenUnknownVersion :: Monad m => Word8 -> m a
accessTokenUnknownVersion v =
  fail $ "fb-persistent: AccessToken: unknown version " ++ show v ++
         ", are you using different versions of fb-persistent?"
