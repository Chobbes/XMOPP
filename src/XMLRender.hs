module XMLRender where

import Data.Conduit

import Data.XML.Types (Event(..), Content(..))
import Text.XML hiding (parseText)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

renderElement :: Element -> BS.ByteString
renderElement = LBS.toStrict . renderLBS (def {rsXMLDeclaration=False}) . elemToDoc

-- | Conduit which converts XML elements into bytestrings.
renderElements :: Monad m => ConduitT Element BS.ByteString m ()
renderElements = do
  e <- await
  case e of
    Nothing -> return ()
    Just e  -> yield $ renderElement e
  renderElements

--------------------------------------------------
-- Constructing XML elements and documents.
--------------------------------------------------

-- | Convert an element into a plain document.
elemToDoc :: Element -> Document
elemToDoc e = Document emptyPrologue e []

-- | Plain, empty prologue for XML.
emptyPrologue :: Prologue
emptyPrologue = Prologue [] Nothing []
