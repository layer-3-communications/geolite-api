{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import           CMark                                hiding ( Url )
import           Control.Monad.IO.Class               ( liftIO )
import           Control.Monad.Trans.Class            ( MonadTrans(..) )
import qualified Data.Aeson                           as AE
import qualified Data.Aeson.Encode.Pretty             as AEP
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.ByteString.Encodings            as BE
import qualified Data.Diet.Map.Strict.Unboxed.Lifted  as D
import           Data.IORef
import qualified Data.Map.Strict                      as MS
import           Data.Maybe                           ( maybe )
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.IO                         as TIO
import qualified Data.Text.Lazy                       as TL
import           GeoliteApi.Siphon                    ( getCsvs, replaceCsvs )
-- TODO: Add explicit import list to GeoliteApi.Types
import           GeoliteApi.Types
import           Net.Types                            ( IPv4, IPv6 )
import           Network.Wai.Middleware.RequestLogger
import           System.Cron
import           System.Mem                           ( performMajorGC )
import           Web.Scotty

main :: IO ()
main = do
  -- At startup, create our 'Maps',
  -- and store them in an 'IORef'.
  putStrLn "Putting together an all-star flight crew..."
  csvRef :: IORef Maps <- getCsvs >>= newIORef
  -- Force a major GC to prevent unnecessary
  -- space usage until 'csvRef' gets overwritten.
  putStrLn "Collecting space garbage..."
  performMajorGC
  -- Add a cron job that periodically downloads
  -- the new CSVs and updates the 'csvRef'.
  _ <- execSchedule $ do
    addJob (replaceCsvs csvRef) "30 2 * * *"
  -- Start the server.
  putStrLn "Liftoff."
  server csvRef

server :: IORef Maps -> IO ()
server imaps = do
  scotty 3000 $ do
    ----- logger
    middleware logStdoutDev
    ----- help page
    get (regex "/help|readme") $ do
      readMe <- liftIO $ TIO.readFile "./README.md"
      html $ sakura <> (TL.fromStrict $ commonmarkToHtml [] readMe)
--    get "" $ do
--      readMe <- liftIO $ TIO.readFile "./README.md"
--      html $ sakura <> (TL.fromStrict $ commonmarkToHtml [] readMe)
    ----- IPv4 queries
    get "/ipv4/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps
      let total = mkTotal csvMaps (Left query)
          encoded :: Maybe T.Text 
          encoded = prettify total
      maybe (text "null") (text . TL.fromStrict) encoded
    ----- IPv6 queries
    get "/ipv6/:ip" $ do
      query <- param "ip"
      csvMaps <- lift $ readIORef imaps
      let total = mkTotal csvMaps (Right query) 
          encoded :: Maybe T.Text 
          encoded = prettify total
      maybe (text "null") (text . TL.fromStrict) encoded 

mkTotal :: Maps -> Either IPv4 IPv6 -> Total
mkTotal csvMaps = \case
  Left i4 ->
    let asn :: Maybe ASN
        asn = D.lookup i4 $ asnipv4diet csvMaps
        country :: Maybe Country 
        country = D.lookup i4 $ countryipv4diet csvMaps
        cityBlock :: Maybe CityBlock 
        cityBlock = D.lookup i4 $ cityBlockipv4diet csvMaps
        countryGid :: Maybe Int
        countryGid = case country of { Nothing -> Nothing; Just (Country g _ _ _ _) -> g; }
        cityGid :: Maybe Int
        cityGid = case cityBlock of { Nothing -> Nothing; Just (CityBlock a _ _ _ _ _ _ _ _) -> boxMaybeInt a; }
        cityLocation :: Maybe CityLocation
        cityLocation = maybe Nothing (\x -> MS.lookup x $ cityLocationMap csvMaps) cityGid
        countryLocation :: Maybe CountryLocation
        countryLocation = maybe Nothing (\x -> MS.lookup x $ countryLocationMap csvMaps) countryGid
    in Total asn country cityBlock cityLocation countryLocation -- (fromMaybe 0 cityGid) (fromMaybe 0 countryGid) (isJust cityLocation) (isJust countryLocation) (length $ cityLocationMap csvMaps) (length $ countryLocationMap csvMaps)
  Right i6 ->
    let asn :: Maybe ASN
        asn = D.lookup i6 $ asnipv6diet csvMaps
        country :: Maybe Country
        country = D.lookup i6 $ countryipv6diet csvMaps
        cityBlock :: Maybe CityBlock
        cityBlock = D.lookup i6 $ cityBlockipv6diet csvMaps
        countryGid :: Maybe Int
        countryGid = case country of { Nothing -> Nothing; Just (Country g _ _ _ _) -> g; }
        cityGid :: Maybe Int
        cityGid = case cityBlock of { Nothing -> Nothing; Just (CityBlock a _ _ _ _ _ _ _ _) -> boxMaybeInt a; }
        cityLocation :: Maybe CityLocation
        cityLocation = maybe Nothing (\x -> MS.lookup x $ cityLocationMap csvMaps) cityGid
        countryLocation :: Maybe CountryLocation
        countryLocation = maybe Nothing (\x -> MS.lookup x $ countryLocationMap csvMaps) countryGid
    in Total asn country cityBlock cityLocation countryLocation -- (fromMaybe 0 cityGid) (fromMaybe 0 countryGid) (isJust cityLocation) (isJust countryLocation) (length $ cityLocationMap csvMaps) (length $ countryLocationMap csvMaps)
    
-- | prettily encode some data to JSON, then
--   make sure it's UTF-8 encoded.
prettify :: AE.ToJSON a => a -> Maybe T.Text
prettify b =
  let encoded = BL.toStrict $ AEP.encodePretty b
  in if BE.isUtf8 encoded
     then Just $ TE.decodeUtf8 encoded
     else Nothing

sakura :: TL.Text
sakura = "<link rel=\"stylesheet\"\
        \ href=\"https://unpkg.com/sakura.css/css/sakura.css\"\
        \ type=\"text/css\">"
