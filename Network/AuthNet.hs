{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module Network.AuthNet (
  AuthNetRequest(..)
 ,AuthNetResponse(..)
 ,AuthNetSettings(..)
 ,AuthType
 ,AuthMethod
 ,postAuthNet
 ,defaultAuthNetRequest
 ,testAuthNetRequest
 ,addCredentials
 ,addField
 ,addFields
 ) where

import Data.Settings
import Network.HTTP (urlEncode)
import Network.Curl 
import Data.List (findIndices)

data AuthNetSettings = AuthNetSettings {
  curlDebug :: Bool
  }

getSettings :: (Settings AuthNetSettings) => AuthNetSettings
getSettings = settings

data AuthType = AuthCapture

instance Show AuthType where
  show AuthCapture = "AUTH_CAPTURE"

data AuthMethod = CreditCard

instance Show AuthMethod where
  show CreditCard = "CC"

data AuthNetRequest = AuthNetRequest {
  -- API
   x_url :: String
  ,x_version :: String
  ,x_delim_data :: Bool
  ,x_delim_char :: Char
  ,x_relay_response :: Bool
  ,x_test_request :: Bool

  -- Credentials
  ,x_login :: String
  ,x_password :: String

  -- Charge
  ,x_amount :: Float
  ,x_description :: String
  
  -- Card
  ,x_type :: AuthType
  ,x_method :: AuthMethod
  ,x_card_num :: Integer
  ,x_exp_date :: Integer

  -- Billing Address
  ,x_first_name :: String
  ,x_last_name :: String
  ,x_address :: String
  ,x_state :: String
  ,x_zip :: Integer

  ,x_extra_fields :: [(String,String)]
  }

data AuthNetResponse = AuthNetResponse {
   responseCode :: Int
  ,responseReasonCode :: Int
  ,responseReason :: String
  }
  deriving (Show)

-- | @defaultAuthNetRequest@ provides basic settings that should
-- work in most cases.  It is meant to be used with real (not test)
-- accounts.
defaultAuthNetRequest = AuthNetRequest {
   x_url = "https://secure.authorize.net/gateway/transact.dll"
  ,x_version = "3.1"
  ,x_delim_data = True
  ,x_delim_char = '|'
  ,x_relay_response = False
  ,x_test_request = False

  ,x_type = AuthCapture
  ,x_method = CreditCard

  -- Undefineds
  ,x_login = undefined
  ,x_password = undefined
  ,x_amount = undefined
  ,x_description = undefined
  ,x_card_num = undefined
  ,x_exp_date = undefined
  ,x_first_name = undefined
  ,x_last_name = undefined
  ,x_address = undefined
  ,x_state = undefined
  ,x_zip = undefined
  ,x_extra_fields = []
  }

-- | @testAuthNetRequest@ uses the same basic settings as 
-- @defaultAuthNetRequest@, but is for test accounts
testAuthNetRequest = defaultAuthNetRequest {
   x_url = "https://test.authorize.net/gateway/transact.dll"
  ,x_test_request = True
  }

addCredentials l p req = req { x_login=l, x_password=p }

parseAuthNetResponse :: AuthNetRequest -> String -> AuthNetResponse
parseAuthNetResponse req str
  = AuthNetResponse {
       responseCode = read $ cols !! 0
      ,responseReasonCode = read $ cols !! 2
      ,responseReason = cols !! 3
      }
  where cols = splitOn (x_delim_char req) str

authNetHeaders req
  = ["x_version=" ++ (cvt $ x_version req)
    ,"x_delim_data=" ++ (cvt $ x_delim_data req)
    ,"x_delim_char=" ++ (cvt $ x_delim_char req)
    ,"x_relay_response=" ++ (cvt $ x_relay_response req)
    ,"x_test_request=" ++ (cvt $ x_test_request req)
  
    -- Credentials
    ,"x_login=" ++ (cvt $ x_login req)
    ,"x_password=" ++ (cvt $ x_password req)

    -- Charge
    ,"x_amount=" ++ (cvt $ x_amount req)
    ,"x_description=" ++ (cvt $ x_description req)
    
    -- Card
    ,"x_type=" ++ (cvt $ x_type req)
    ,"x_method=" ++ (cvt $ x_method req)
    ,"x_card_num=" ++ (cvt $ x_card_num req)
    ,"x_exp_date=" ++ (cvt $ x_exp_date req)

    -- Billing Address
    ,"x_first_name=" ++ (cvt $ x_first_name req)
    ,"x_last_name=" ++ (cvt $ x_last_name req)
    ,"x_address=" ++ (cvt $ x_address req)
    ,"x_state=" ++ (cvt $ x_state req)
    ,"x_zip=" ++ (cvt $ x_zip req)
    ] ++ (map cvt_extra $ x_extra_fields req)
  
  where cvt :: (ToString a) => a -> String
        cvt = urlEncode.toString
        cvt_extra (k,v) = (urlEncode k) ++ "=" ++ (urlEncode v)

class ToString a where
  toString :: a -> String

instance ToString Char where
  toString x = [x]

instance ToString [Char] where
  toString = id

instance (Show a) => ToString a where
  toString = show

addFields :: AuthNetRequest -> [(String,String)] -> AuthNetRequest
addFields req@(AuthNetRequest { x_extra_fields = old }) new = req { x_extra_fields = old ++ new }

addField :: AuthNetRequest -> (String,String) -> AuthNetRequest
addField req new = addFields req [new]

-- | 'curlPost' performs. a common POST operation, namely that
-- of submitting a sequence of name=value pairs.
--curlPostResponse :: (CurlHeader hdr, CurlBuffer ty) => URLString -> [String] -> IO (CurlResponse_ hdr ty)
curlPostResponse :: Bool -> URLString -> [String] -> IO CurlResponse
curlPostResponse debug s ps = initialize >>= \ h -> do
  setopt h (CurlVerbose debug)
  setopt h (CurlPostFields ps)
  setopt h (CurlCookieJar "cookies")
  setopt h (CurlURL s)
  perform_with_response_ h

postAuthNet :: (Settings AuthNetSettings) => AuthNetRequest -> IO AuthNetResponse
postAuthNet req 
  = do res <- curlPostResponse debug (x_url req) (authNetHeaders req)
       return . parseAuthNetResponse req $ respBody res
  where (AuthNetSettings debug) = getSettings

splitAtL = splitAtL' 0
splitAtL' _ [] xs = [xs]
splitAtL' off (i:is) xs 
  = front : splitAtL' (i+1) is back
  where (front,_:back) = splitAt (i-off) xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f xs = splitAtL (findIndices f xs) xs
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn a = splitBy (==a)

