{-# LANGUAGE OverloadedStrings #-}

module Web.JonathansCard
    (
      Balance(..)
    , Change(..)
    , balances
    , latest
    , changes

    {- Re-Export -}
    , UTCTime
    ) where

import Control.Monad         ( ap, liftM )
import Data.ByteString.Char8 ( ByteString, unpack )
import Data.Time.Clock       ( UTCTime )
import Data.Time.Format      ( parseTime )
import Network.HTTP          ( Header(Header), HeaderName(..), Request(Request)
                             , RequestMethod(GET), getResponseBody, simpleHTTP
                             , catchIO
                             )
import Network.URI           ( URI(URI), URIAuth(URIAuth) )
import System.Locale         ( defaultTimeLocale )
import Text.JSON             ( JSON(..), JSObject(..), JSValue(..), Result
                             , decode, resultToEither, valFromObj
                             )

-- | Represents a Balance on Jonathan's Card
data Balance = Balance
    { balAmount     :: Double
    , balBalanceId  :: Int
    , balCreated    :: Maybe UTCTime
    , balMessage    :: String
    } deriving Show

-- | Represents the changes over time on Jonathan's Card
data Change = Change
    { chgBalance    :: Double
    , chgCreated    :: Maybe UTCTime
    , chgDelta      :: Double
    } deriving Show

-- | Retrieve a list of balances on Jonathan's Card
balances :: IO (Either String [Balance])
balances  = apiCall "balances" "balances" "balances"

-- | Retrieve the latest balance on Jonathan's Card
latest :: IO (Either String Balance)
latest  = apiCall "latest" "balance" "latest balance"

-- | Retrieve the changes in amounts on Jonathan's Card
changes :: IO (Either String [Change])
changes  = apiCall "changes" "changes" "changes"

-- | Abstracts the details of the API call and body parsing.
apiCall :: JSON a => String -> String -> String -> IO (Either String a)
apiCall endpoint parent errObject = flip catchIO (\_ -> err) $ do
    rsp <- (decode . unpack) `liftM` request endpoint
    return . resultToEither $ valFromObj parent =<< rsp
    where err = return . Left $ "Unable to retrieve " ++ errObject ++ "."

-----------------------
-- Request Utilities --
-----------------------

-- | Sends response to server
request  :: String -> IO ByteString
request p = (simpleHTTP $ prepRq p) >>= getResponseBody

-- | Prepare request to API
prepRq  :: String -> Request ByteString
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

-- | Attempts to parse a string into a UTCTime.
getTime :: String -> Maybe UTCTime
getTime  = parseTime defaultTimeLocale "%Y-%m-%dT%X%z"

instance JSON Balance where
    readJSON (JSObject rsp) = do
        Balance  `liftM` (read    `liftM` get rsp "amount")
                    `ap` (read    `liftM` get rsp "balance_id")
                    `ap` (getTime `liftM` get rsp "created_at")
                    `ap` get rsp "message"
    readJSON _ = undefined
    showJSON   = undefined

instance JSON Change where
    readJSON (JSObject rsp) = do
        Change   `liftM` (read    `liftM` get rsp "balance")
                    `ap` (getTime `liftM` get rsp "created_at")
                    `ap` (read    `liftM` get rsp "delta")
    readJSON _ = undefined
    showJSON   = undefined
