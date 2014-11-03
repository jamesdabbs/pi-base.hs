module Middleware
( cors
) where

import Prelude

import Network.HTTP.Types (ResponseHeaders, Header)
import Network.Wai (Middleware)
import Network.Wai.Internal (Response(..))


cors :: Middleware
cors = withHeader ("Access-Control-Allow-Origin", "http://localhost:8000")

withHeader :: Header -> Middleware
withHeader h app req respond = app req $ respond . addHeader h

mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseStream s h b) = ResponseStream s (f h) b
mapHeader _ r@(ResponseRaw _ _) = r

addHeader :: Header -> Response -> Response
addHeader h = mapHeader (\hs -> h:hs)
