{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This package is meant to be imported as
--
-- > import Facebook.Persistent ()
--
-- because it doesn't export any symbols.  However, it exports
-- the following orphan instances:
--
-- @
-- instance "PersistField" "Action" -- since 0.1
-- @
module Facebook.Persistent () where

import Database.Persist
import Facebook

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
