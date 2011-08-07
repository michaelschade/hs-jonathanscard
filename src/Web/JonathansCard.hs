{-# LANGUAGE OverloadedStrings #-}

module Web.JonathansCard
    (
      Balance(..)
    , balances
    , latest
    ) where

import Control.Monad        ( ap, liftM )
import Data.Maybe           ( fromMaybe )
import Data.Text.Encoding   ( decodeUtf8, encodeUtf8 )
import Text.JSON
import Network.HTTP         ( Header(Header), HeaderName(..), Request(Request)
                            , RequestMethod(GET), getResponseBody, simpleHTTP
                            , catchIO
                            )
import Network.URI          ( URI(URI), URIAuth(URIAuth) )
import qualified Data.ByteString.Char8  as C8

-- | Represents a Balance on Jonathan's Card
data Balance = Balance
    { amount    :: Double
    , balanceId :: Int
    , created   :: String
    , message   :: String
    } deriving Show

-- | Retrieve a list of balances on Jonathan's Card
balances :: IO (Either String [Balance])
balances  = flip catchIO (\_ -> err) $ do
    rsp <- (decode . C8.unpack) `liftM` request "balances"
    return . resultToEither $ valFromObj "balances" =<< rsp
    where err = return . Left $ "Unable to retrieve balances."

-- | Retrieve the latest balance on Jonathan's Card
latest :: IO (Either String Balance)
latest  = flip catchIO (\_ -> err) $ do
    rsp <- (decode . C8.unpack) `liftM` request "latest"
    return . resultToEither $ valFromObj "balance" =<< rsp
    where err = return . Left $ "Unable to retrieve latest balance."

-----------------------
-- Request Utilities --
-----------------------

-- | Sends response to server
request  :: String -> IO C8.ByteString
request p = (simpleHTTP $ prepRq p) >>= getResponseBody

-- | Prepare request to API
prepRq  :: String -> Request C8.ByteString
prepRq p = Request (url p) GET headers ""
    where
        headers = [ Header HdrAccept    "application/json"
                  , Header HdrUserAgent "JonathansCard/0.1 haskell"
                  ]

-- | Define request URL based on endpoint
url  :: String -> URI
url p = URI "http:" uriAuth ("/card/api/" ++ p) "" ""
    where uriAuth = Just $ URIAuth "" "jonathanstark.com" ":80"

----------------------------
-- JSON Parsing Utilities --
----------------------------

-- | Convenient name to get a field from a given 'JSON' object.
get :: JSON a => JSObject JSValue -> String -> Result a
get  = flip valFromObj

instance JSON Balance where
    readJSON (JSObject rsp) = do
        Balance  `liftM` (read `liftM` get rsp "amount")
                    `ap` (read `liftM` get rsp "balance_id")
                    `ap` get rsp "created_at"
                    `ap` get rsp "message"
    readJSON _ = undefined
    showJSON   = undefined
