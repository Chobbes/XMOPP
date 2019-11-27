{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Stream where

import Data.Conduit
import Data.UUID
import Data.Text (Text)
import Data.Default
import System.Random
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Primitive
import Data.XML.Types (Event(..), Content(..))
import Text.XML hiding (parseText)
import qualified Data.ByteString as BS
import qualified Text.XML.Stream.Render as XR

import XMPP

awaitName :: MonadThrow m => Name -> ConduitT Event o m (Maybe Event)
awaitName name = do
  element <- await
  case element of
    Just (EventBeginElement n _) ->
      if n == name
        then return element
        else awaitName name
    Nothing -> return Nothing
    _ -> awaitName name

awaitStream :: MonadThrow m => ConduitT Event o m (Maybe Event)
awaitStream = awaitName (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})

streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId =
  yield $ EventBeginElement streamName attrs
  where attrs = [ at "from" from
                , at "version" version
                , at "id" (toText streamId)
                , (Name "lang" (Just "xml") (Just "xml"), [ContentText lang]) ]
        at name content = (Name name Nothing Nothing, [ContentText content])
        streamName = Name "stream"
                          (Just "http://etherx.jabber.org/streams")
                          (Just "stream")

-- | Open a stream.
openStream
  :: (MonadThrow m, PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadIO m) =>
     ConduitT i Event m () ->
     ConduitT BS.ByteString o m r ->  -- ^ Need to use BS because XML renderer doesn't flush.
     ConduitT i o m UUID
openStream source sink = do
  source .| awaitStream
  liftIO $ putStrLn "Got connection stream thing..."
  initiateStream sink

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m) =>
  ConduitT BS.ByteString o m r -> ConduitT i o m UUID
initiateStream sink = do
    streamId <- liftIO randomIO
    fqdn <- asks fqdn
    liftIO $ putStrLn "Sending stream response..."
    streamRespHeader fqdn "en" streamId .| XR.renderBytes def .| sink
    liftIO $ putStrLn "Done..."
    return streamId

features :: [Node] -> Element
features nodes = Element featureName mempty nodes
  where
    featureName = Name "features"
                       (Just "http://etherx.jabber.org/streams")
                       (Just "stream")

required :: Element
required = Element "required" mempty []
