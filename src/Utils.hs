module Utils where

import Conduit

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))

-- | Skip to the end element of a tag, consuming everything before it, and
-- yielding the end element and the rest of the stream after it.
skipToEnd :: Monad m => Name -> ConduitT Event Event m ()
skipToEnd name = do
  e <- await
  case e of
    Nothing -> return ()
    Just e  ->
      if e == EventEndElement name
      then yield e >> awaitForever yield
      else skipToEnd name
