{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}  -- Persistent :(?

module Users where

import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

-- Want plugins to be able to add state to users as well.
-- Roster?

-- TODO make sure passwords aren't plain text in the future.
-- | Data type for XMPP users.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text
    UniqueName name
    deriving Show
|]
