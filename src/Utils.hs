{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Conduit
import Data.Conduit.Combinators
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.Text (Text)
import Data.XML.Types (Event(..), Content(..))
import qualified Data.Map as M

iqName :: Name
iqName = Name {nameLocalName = "iq", nameNamespace = Just "jabber:client", namePrefix = Nothing}

iq :: Text -> Text -> Text -> Text -> [Node] -> Element
iq i t to from = Element iqName attrs
  where attrs = M.fromList [("id", i), ("type", t), ("to", to), ("from", from)]

queryName :: Text -> Name
queryName namespace = Name {nameLocalName = "query", nameNamespace = Just namespace, namePrefix = Nothing}

query :: Text -> [Node] -> Element
query namespace = Element (queryName namespace) mempty

presenceName :: Name
presenceName = Name {nameLocalName = "presence", nameNamespace = Just "jabber:client", namePrefix = Nothing}

-- | Skip to the end element of a tag, consuming everything before it.
skipToEnd :: (Monad m, MonadThrow m) => Name -> ConduitT Event o m ()
skipToEnd name = do
  e <- peek
  case e of
    Nothing -> return ()
    Just e  ->
      if e == EventEndElement name
      then return ()
      else do
        ignoreAnyTreeContent
        skipToEnd name
