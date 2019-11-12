{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Network
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Data.Conduit.List as CL
import Data.Char

xmppPort :: Int
xmppPort = 5222

settings :: ServerSettings
settings = serverSettings xmppPort "*"

handleClient :: AppData -> IO ()
handleClient ad =
  runConduit $ do
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ putStrLn "New connection"
  yield streamResp .| appSink ad
  appSource ad .| parseBytes def .| awaitForever (lift . print)
  liftIO $ print stream



data Stream = MkStream { _streamVersion :: Text
                       , _streamTo      :: Text
                       , _streamId      :: Maybe Text
                       }
              deriving (Show, Eq)

-- parseStream :: MonadThrow m => ConduitT Event o m (Maybe [Text])
-- parseStream = tag' "stream" parseAttributes $ \(to, version) -> manyYield parseChats
--       where parseAttributes = (,) <$> requireAttr "to" <*> requireAttr "version" <* ignoreAttrs


parseStream :: MonadThrow m => ConduitT Event Text m ()
parseStream = force "blah" (tagIgnoreAttrs "stream" (manyYield parseChats))

parseChats :: MonadThrow m => ConduitT Event o m (Maybe Text)
parseChats = tagIgnoreAttrs "chat" content

chatTest :: Text
chatTest = "<chat>yay</chat>"

xmlTestSmall :: Text
xmlTestSmall = "<stream:stream version='1.0' to='blah2'><chat>yay</chat><chat>wooo</chat><stream:stream version='1.0' to='blah3'><chat>yay2</chat><chat>wooo2</chat></stream:stream><stream:stream version='1.0' to='blah2'><chat>yay</chat><chat>wooo</chat>"

xmlTest :: Text
xmlTest =
  "<?xml version='1.0'?> \
\      <stream:stream  \
\          from='juliet@im.example.com'  \
\          to='im.example.com'  \
\          version='1.0'  \
\          xml:lang='en'  \
\          xmlns='jabber:client'  \
\          xmlns:stream='http://etherx.jabber.org/streams'>" 

--streamResp :: ByteString
-- TODO fix this. Generate id randomly.
streamResp =
  "<?xml version='1.0'?> \
\      <stream:stream  \
\          from='localhost'  \
\          id='FOO++TR84Sm6A3hnt3Q065SnAbbk3Y=' \
\          version='1.0'  \
\          xml:lang='en'  \
\          xmlns='jabber:client'  \
\          xmlns:stream='http://etherx.jabber.org/streams'> \
\ <features xml:lang='en'  \
\           xmlns='jabber:client'  \
\           xmlns:stream='http://etherx.jabber.org/streams'> \
\ <starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"> \
\ <required /> \
\ </starttls> \
\ </features>"


test :: IO ()
test = runConduit $ yield xmlTestSmall .| parseText def .| parseStream .| awaitForever (lift . print)

streamBegin :: MonadThrow m => r -> ConduitT Event a m r -> ConduitT Event a m r
streamBegin r rest = do
  element <- await
  case element of
    Just e@(EventBeginElement n ats) -> do
      if n == (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})
        then rest >> streamBegin r rest
        else streamBegin r rest
    Just (EventEndElement n) -> do
      if n == "stream"
        then return r  -- Close stream with default value
        else streamBegin r rest
    Nothing -> return r
    _ -> streamBegin r rest

awaitStream :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStream = do
  element <- await
  case element of
    Just (EventBeginElement n ats) ->
      if n == (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})
        then return element
        else awaitStream
    Nothing -> return Nothing
    _ -> awaitStream

main :: IO ()
main = runTCPServer settings handleClient


-- -- | The most generic way to parse a tag. It takes a 'NameMatcher' to check whether
-- -- this is a correct tag name, an 'AttrParser' to handle attributes, and
-- -- then a parser to deal with content.
-- --
-- -- 'Events' are consumed if and only if the tag name and its attributes match.
-- --
-- -- This function automatically absorbs its balancing closing tag, and will
-- -- throw an exception if not all of the attributes or child elements are
-- -- consumed. If you want to allow extra attributes, see 'ignoreAttrs'.
-- --
-- -- This function automatically ignores comments, instructions and whitespace.
-- myTag :: MonadThrow m
--     => NameMatcher a -- ^ Check if this is a correct tag name
--                      --   and return a value that can be used to get an @AttrParser@.
--                      --   If this fails, the function will return @Nothing@
--     -> (a -> AttrParser b) -- ^ Given the value returned by the name checker, this function will
--                            --   be used to get an @AttrParser@ appropriate for the specific tag.
--                            --   If the @AttrParser@ fails, the function will also return @Nothing@
--     -> (b -> ConduitT Event o m c) -- ^ Handler function to handle the attributes and children
--                                    --   of a tag, given the value return from the @AttrParser@
--     -> ConduitT Event o m (Maybe c)
-- myTag nameMatcher attrParser f = do
--   (x, leftovers) <- dropWS []
--   res <- case x of
--     Just (EventBeginElement name as) -> case runNameMatcher nameMatcher name of
--       Just y -> case runAttrParser' (attrParser y) as of
--         Left _ -> return Nothing
--         Right z -> do
--           z' <- f z
--           (a, _leftovers') <- dropWS []
--           case a of
--             Just (EventEndElement name')
--               | name == name' -> return (Just z')
--             _ -> lift $ throwM $ InvalidEndElement name a
--       Nothing -> return Nothing
--     _ -> return Nothing

--   case res of
--     -- Did not parse, put back all of the leading whitespace events and the
--     -- final observed event generated by dropWS
--     Nothing -> Prelude.mapM_ leftover leftovers
--     -- Parse succeeded, discard all of those whitespace events and the
--     -- first parsed event
--     Just _  -> return ()

--   return res
--   where
--     -- Drop Events until we encounter a non-whitespace element. Return all of
--     -- the events consumed here (including the first non-whitespace event) so
--     -- that the calling function can treat them as leftovers if the parse fails
--     dropWS leftovers = do
--         x <- await
--         let leftovers' = maybe id (:) x leftovers

--         case isWhitespace <$> x of
--           Just True -> dropWS leftovers'
--           _         -> return (x, leftovers')
--     runAttrParser' p as =
--         case runAttrParser p as of
--             Left e           -> Left e
--             Right ([], x)    -> Right x
--             Right (attr', _) -> Left $ toException $ UnparsedAttributes attr'

-- -- | A simplified version of 'tag' where the 'NameMatcher' result isn't forwarded to the attributes parser.
-- --
-- -- Since 1.5.0
-- myTag' :: MonadThrow m
--      => NameMatcher a -> AttrParser b -> (b -> ConduitT Event o m c)
--      -> ConduitT Event o m (Maybe c)
-- myTag' a b = tag a (const b)


-- isWhitespace :: Event -> Bool
-- isWhitespace EventBeginDocument             = True
-- isWhitespace EventEndDocument               = True
-- isWhitespace EventBeginDoctype{}            = True
-- isWhitespace EventEndDoctype                = True
-- isWhitespace EventInstruction{}             = True
-- isWhitespace (EventContent (ContentText t)) = T.all isSpace t
-- isWhitespace EventComment{}                 = True
-- isWhitespace (EventCDATA t)                 = T.all isSpace t
-- isWhitespace _                              = False
