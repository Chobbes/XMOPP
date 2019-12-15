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
import Control.Monad.Logger
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

streamName :: Name
streamName = Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"}

awaitStream :: MonadThrow m => ConduitT Event o m (Maybe Event)
awaitStream = awaitName streamName

streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId =
  yield $ EventBeginElement streamName attrs
  where attrs = [ at "from" from
                , at "version" version
                , at "id" (toText streamId)
                , (Name "lang" (Just "xml") (Just "xml"), [ContentText lang]) ]
        at name content = (Name name Nothing Nothing, [ContentText content])

-- TODO: right now we only handle clients opening the stream, since the message sent when opening the stream doesn't include the UUID. These functions handle replying to the client who sends the first message.
-- | Open a stream with a random UUID
openStreamIO
  :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m, MonadLogger m, MonadIO m) =>
     ConduitT i Event m () ->
     ConduitT BS.ByteString o m r ->  -- ^ Need to use BS because XML renderer doesn't flush.
     ConduitT i o m UUID
openStreamIO source sink = do
  streamId <- liftIO randomIO
  openStream streamId source sink

-- | Open a stream with a UUID.
openStream
  :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m, MonadLogger m) =>
     UUID ->
     ConduitT i Event m () ->
     ConduitT BS.ByteString o m r ->  -- ^ Need to use BS because XML renderer doesn't flush.
     ConduitT i o m UUID
openStream uuid source sink = do
  stream <- source .| awaitStream -- TODO check if stream == Nothing. Then maybe we can send a stream without a UUID?
  logDebugN "Got connection stream..."
  initiateStream uuid sink

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadReader XMPPSettings m, MonadLogger m) =>
  UUID -> ConduitT BS.ByteString o m r -> ConduitT i o m UUID
initiateStream streamId sink = do
    fqdn <- asks fqdn
    logDebugN "Sending stream response..."
    streamRespHeader fqdn "en" streamId .| XR.renderBytes def .| sink
    logDebugN "Done stream response..."
    return streamId

features :: [Node] -> Element
features = Element featureName mempty
  where
    featureName = Name "features"
                       (Just "http://etherx.jabber.org/streams")
                       (Just "stream")

required :: Element
required = Element "required" mempty []
