{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}  -- Persistent :(?

module Users where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Text
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)

-- Want plugins to be able to add state to users as well.
-- Roster?

-- TODO make sure passwords aren't plain text in the future.
-- | Data type for XMPP users.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text
    UniqueName name
    deriving Show Eq
Roster
    owner UserId
    name Text
    deriving Show Eq
|]

-- | Insert a list of users into the database.
insertUsers
  :: (PersistUniqueWrite backend, MonadIO m, Foldable t,
      AtLeastOneUniqueKey record,
      PersistEntityBackend record ~ BaseBackend backend) =>
     t record -> ReaderT backend m ()
insertUsers = mapM_ insertBy
