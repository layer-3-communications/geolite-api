{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           CMark                                hiding ( Url )
import           Codec.Archive.Zip
import           Colonnade                            ( Headed )
import           Control.Concurrent
import           Control.DeepSeq                      ( NFData (rnf) )
import           Control.Exception                    ( AsyncException (..),
                                                        evaluate )
import           Control.Monad                        ( forever, guard, (<=<) )
import           Control.Monad.IO.Class               ( liftIO )
import           Control.Monad.Trans.Class            ( MonadTrans(..) )
import qualified Country                              as C
import qualified Data.Aeson                           as AE
import qualified Data.ByteString.Char8                as B
import           Data.ByteString.Lazy                 ( fromStrict )
import qualified Data.ByteString.Streaming            as BS
import           Data.Compact                         ( compact, getCompact )
import           Data.Default
import qualified Data.Diet.Map.Unboxed.Lifted         as D
import           Data.IORef
import qualified Data.Map.Strict                      as MS
import           Data.Maybe                           ( fromJust )
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.IO                         as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.IO                    as TL
import           Data.Text.Short                      ( ShortText )
import qualified Data.Text.Short                      as TS
import           GeoliteApi.Types
import           Net.IPv4                             ( IPv4 )
import qualified Net.IPv4                             as IPv4
import           Net.IPv6                             ( IPv6 )
import qualified Net.IPv6                             as IPv6
import           Network.HTTP.Req
import           Network.Wai.Middleware.RequestLogger
import qualified Siphon                               as SI
import qualified Streaming                            as SR
import qualified Streaming.Prelude                    as SR
import           System.Cron
import           System.Cron.Schedule                 ( ScheduleT(..) )
import           System.Directory
import qualified System.IO                            as IO
import           System.Mem                           ( performMajorGC )
import           Web.Scotty

-- functions used by siphon to decode csvs
siphonAsn :: SI.Siphon Headed B.ByteString (IPv4, IPv4, ASN)
siphonAsn = (\r asn -> (IPv4.lowerInclusive r,IPv4.upperInclusive r,asn))
  <$> SI.headed "network" ipv4Decode
  <*> ( ASN
        <$> SI.headed "autonomous_system_number" numDecode
        <*> SI.headed "autonomous_system_organization" shortTextDecode
      )

siphonAsnv6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, ASN)
siphonAsnv6 = (\r asn -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,asn))
  <$> SI.headed "network" ipv6Decode
  <*> ( ASN
        <$> SI.headed "autonomous_system_number" numDecode
        <*> SI.headed "autonomous_system_organization" shortTextDecode
      )

siphonCountry :: SI.Siphon Headed B.ByteString (IPv4, IPv4, Country)
siphonCountry = (\r c -> (IPv4.lowerInclusive r,IPv4.upperInclusive r,c))
  <$> SI.headed "network" ipv4Decode
  <*> ( Country
        <$> SI.headed "geoname_id" numDecode
        <*> SI.headed "registered_country_geoname_id" numDecode
        <*> SI.headed "represented_country_geoname_id" numDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
      )

siphonCountryV6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, Country)
siphonCountryV6 = (\r c -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,c))
  <$> SI.headed "network" ipv6Decode
  <*> ( Country
        <$> SI.headed "geoname_id" numDecode
        <*> SI.headed "registered_country_geoname_id" numDecode
        <*> SI.headed "represented_country_geoname_id" numDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
      )

siphonCountryLocations :: SI.Siphon Headed B.ByteString (Maybe Int, CountryLocation)
siphonCountryLocations = (\g cl -> (g,cl))
  <$> SI.headed "geoname_id" numDecode
  <*> ( CountryLocation
        <$> SI.headed "locale_code" shortTextDecode
        <*> SI.headed "continent_code" shortTextDecode
        <*> SI.headed "continent_name" shortTextDecode
        <*> SI.headed "country_iso_code" C.decodeUtf8
        <*> SI.headed "country_name" C.decodeUtf8
        <*> SI.headed "is_in_european_union" boolDecode
      )

siphonCityBlock :: SI.Siphon Headed B.ByteString (IPv4, IPv4, CityBlock)
siphonCityBlock = (\r cb -> (IPv4.lowerInclusive r,IPv4.upperInclusive r,cb))
  <$> SI.headed "network" ipv4Decode
  <*> ( CityBlock
        <$> SI.headed "geoname_id" maybeNumDecode
        <*> SI.headed "registered_country_geoname_id" maybeNumDecode
        <*> SI.headed "represented_country_geoname_id" maybeNumDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
        <*> SI.headed "postal_code" shortTextDecode
        <*> SI.headed "latitude" maybeNumDecode
        <*> SI.headed "longitude" maybeNumDecode
        <*> SI.headed "accuracy_radius" maybeNumDecode
      )

siphonCityBlockV6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, CityBlock)
siphonCityBlockV6 = (\r cb -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,cb))
  <$> SI.headed "network" ipv6Decode
  <*> ( CityBlock
        <$> SI.headed "geoname_id" maybeNumDecode
        <*> SI.headed "registered_country_geoname_id" maybeNumDecode
        <*> SI.headed "represented_country_geoname_id" maybeNumDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
        <*> SI.headed "postal_code" shortTextDecode
        <*> SI.headed "latitude" maybeNumDecode
        <*> SI.headed "longitude" maybeNumDecode
        <*> SI.headed "accuracy_radius" maybeNumDecode
      )

siphonCityLocations :: SI.Siphon Headed B.ByteString (Maybe Int, CityLocation)
siphonCityLocations = (\g cl -> (g,cl))
  <$> SI.headed "geoname_id" numDecode
  <*> ( CityLocation
        <$> SI.headed "locale_code" shortTextDecode
        <*> SI.headed "continent_code" shortTextDecode
        <*> SI.headed "continent_name" shortTextDecode
        <*> SI.headed "country_iso_code" C.decodeUtf8
        <*> SI.headed "country_name" C.decodeUtf8
        <*> SI.headed "subdivision_1_iso_code" shortTextDecode
        <*> SI.headed "subdivision_1_name" shortTextDecode
        <*> SI.headed "subdivision_2_iso_code" shortTextDecode
        <*> SI.headed "subdivision_2_name" shortTextDecode
        <*> SI.headed "city_name" shortTextDecode
        <*> SI.headed "metro_code" shortTextDecode
        <*> SI.headed "time_zone" shortTextDecode
        <*> SI.headed "is_in_european_union" boolDecode
      )

-- functions used in parsing the data read from the csvs
intToBool :: Int -> FatBool
intToBool 0 = FatFalse
intToBool 1 = FatTrue
intToBool _ = NotTrueOrFalse

readIntExactly :: B.ByteString -> Maybe Int
readIntExactly bs = do
  (ident,remaining) <- B.readInt bs
  guard (B.null remaining)
  return ident

shortTextDecode :: B.ByteString -> Maybe ShortText
shortTextDecode = TS.fromByteString

numDecode :: B.ByteString -> Maybe (Maybe Int)
numDecode  = Just . readIntExactly

maybeNumDecode :: B.ByteString -> Maybe MaybeInt
maybeNumDecode  = Just . unboxMaybeInt . readIntExactly

boolDecode :: B.ByteString -> Maybe FatBool
boolDecode = fmap intToBool . readIntExactly

ipv4Decode :: B.ByteString -> Maybe IPv4.IPv4Range
ipv4Decode = IPv4.decodeRange <=< either (const Nothing) Just . TE.decodeUtf8'
ipv6Decode :: B.ByteString -> Maybe IPv6.IPv6Range
ipv6Decode = IPv6.decodeRange <=< either (const Nothing) Just . TE.decodeUtf8'

--C.decodeAlphaTwo
--C.decode
--C.decodeUtf8

mkBlock ::
     (NFData a, Show a) =>
     FilePath
  -> SI.Siphon Headed B.ByteString a
  -> IO (SR.Of [a] (Maybe SI.SiphonError))
mkBlock path s = IO.withFile path IO.ReadMode
  (\handle -> SR.toList $ SR.mapM (\ a -> evaluate (rnf a) >> pure a) $
      SI.decodeCsvUtf8 s (BS.toChunks $ BS.fromHandle handle))

-- filepaths of csv
asnipv4path :: FilePath
asnipv4path = "./geolite2/asn/GeoLite2-ASN-Blocks-IPv4.csv"
asnipv6path :: FilePath
asnipv6path = "./geolite2/asn/GeoLite2-ASN-Blocks-IPv6.csv"
countryipv4path :: FilePath
countryipv4path = "./geolite2/country/GeoLite2-Country-Blocks-IPv4.csv"
countryipv6path :: FilePath
countryipv6path = "./geolite2/country/GeoLite2-Country-Blocks-IPv6.csv"
cityBlockipv4path :: FilePath
cityBlockipv4path = "./geolite2/city/GeoLite2-City-Blocks-IPv4.csv"
cityBlockipv6path :: FilePath
cityBlockipv6path = "./geolite2/city/GeoLite2-City-Blocks-IPv6.csv"
cityLocationpath :: FilePath
cityLocationpath = "./geolite2/city/GeoLite2-City-Locations-en.csv"
countryLocationpath :: FilePath
countryLocationpath = "./geolite2/country/GeoLite2-Country-Locations-en.csv"
csvPath :: B.ByteString
csvPath = "./geolite2/"

sakura :: TL.Text
sakura = "<link rel=\"stylesheet\" href=\"https://unpkg.com/sakura.css/css/sakura.css\" type=\"text/css\">"

intFromJust :: Maybe Int -> Int
intFromJust (Just a) = a
intFromJust  Nothing = 0
-----------------------
dataf :: IO (Maps)
dataf = do
  -- binds the decoded csv
  asnipv4@          ( asnls SR.:> _            ) <- mkBlock asnipv4path          siphonAsn
  asnipv6@          ( asnv6ls SR.:> _          ) <- mkBlock asnipv6path          siphonAsnv6
  countryipv4@      ( countryls SR.:> _        ) <- mkBlock countryipv4path      siphonCountry
  countryipv6@      ( countryv6ls SR.:> _      ) <- mkBlock countryipv6path      siphonCountryV6
  cityBlockipv4@    ( cityBlockls SR.:> _      ) <- mkBlock cityBlockipv4path    siphonCityBlock
  cityBlockipv6@    ( cityBlockv6ls SR.:> _    ) <- mkBlock cityBlockipv6path    siphonCityBlockV6
  cityLocation@     ( cityLocations SR.:> _    ) <- mkBlock cityLocationpath     siphonCityLocations
  countryLocation@  ( countryLocations SR.:> _ ) <- mkBlock countryLocationpath  siphonCountryLocations

  -- converts the csv to a diet map
  let asnipv4diet        :: D.Map IPv4 ASN                     = D.fromList  asnls
      asnipv6diet        :: D.Map IPv6 ASN                     = D.fromList  asnv6ls
      countryipv4diet    :: D.Map IPv4 Country                 = D.fromList  countryls
      countryipv6diet    :: D.Map IPv6 Country                 = D.fromList  countryv6ls
      cityBlockipv4diet  :: D.Map IPv4 CityBlock               = D.fromList  cityBlockls
      cityBlockipv6diet  :: D.Map IPv6 CityBlock               = D.fromList  cityBlockv6ls
      cityLocationMap    :: MS.Map (Maybe Int) CityLocation    = MS.fromList cityLocations
      countryLocationMap :: MS.Map (Maybe Int) CountryLocation = MS.fromList countryLocations

  x <- compact (Maps asnipv4diet asnipv6diet countryipv4diet
        countryipv6diet cityBlockipv4diet cityBlockipv6diet
        cityLocationMap countryLocationMap)
  pure (getCompact x)

unzipCsvs :: Archive -> Archive -> Archive -> IO ()
unzipCsvs asnZip cityZip countryZip = do
  let zipPath :: FilePath
      zipPath = "./geolite2/"
  extractFilesFromArchive [ OptRecursive, OptDestination $ zipPath ] asnZip
  extractFilesFromArchive [ OptRecursive, OptDestination $ zipPath ] cityZip
  extractFilesFromArchive [ OptRecursive, OptDestination $ zipPath ] countryZip

-- fromJustUrl :: _
-- (the actual type signature here doesn't compiler for whatever reason)
fromJustUrl (Just a) = a
fromJustUrl Nothing  = fromJust $ parseUrlHttps $ B.pack "fail.org"

download :: FilePath -> IO (B.ByteString)
download dlurl = runReq def $ do
  let (url, options) = fromJust (parseUrlHttps $ B.pack dlurl)
  response <- req GET url NoReqBody bsResponse mempty
  pure (responseBody response)

downloadCsvs :: IO (B.ByteString, B.ByteString, B.ByteString)
downloadCsvs = do
  cityZip    <- download "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  countryZip <- download "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country-CSV.zip"
  asnZip     <- download "https://geolite.maxmind.com/download/geoip/database/GeoLite2-ASN-CSV.zip"
  pure (cityZip, countryZip, asnZip)

removeCsvs :: IO ()
removeCsvs = do
  dirs <- listDirectory "./geolite2"
  removes <- mapM_ removeDirectoryRecursive ( map ((<>) "./geolite2/") dirs)
  pure ()

renameDirs :: [FilePath] -> IO ()
renameDirs [a,b,c] = do
  renameDirectory ( "./geolite2/" <> a ) "./geolite2/country/"
  renameDirectory ( "./geolite2/" <> b ) "./geolite2/asn/"
  renameDirectory ( "./geolite2/" <> c ) "./geolite2/city/"
renameDirs _ = error "possible api change"

organizeCsvs :: IO ()
organizeCsvs = do
  dirs <- listDirectory "./geolite2/"
  renameDirs dirs

{-# NOINLINE csvIORef #-}
csvIORef :: IO (IORef Maps)
csvIORef = do 
  x <- getCsvs
  newIORef x

getCsvs :: IO Maps
getCsvs = do
  removeCsvs
  (cityZip, countryZip, asnZip) <- downloadCsvs
  unzipCsvs  ( toArchive $ fromStrict asnZip     )
             ( toArchive $ fromStrict cityZip    )
             ( toArchive $ fromStrict countryZip )
  organizeCsvs
  x <- dataf
  pure (x)

replaceCsvs :: IORef Maps -> IO ()
replaceCsvs imaps = do
  getCsvs >>= writeIORef imaps
  T.putStrLn "Reloading geolite2 data..."
  performMajorGC

-- web server
main :: IO ()
main = do
  csvRef :: IORef Maps <- getCsvs >>= newIORef
  performMajorGC
  cron <- execSchedule $ do
    addJob (replaceCsvs csvRef) "*/2 * * * *"
  readIORef csvRef >>= server
  pure ()

server:: Maps -> IO ()
server x = do
  scotty 3000 $ do
    ----- logger
    middleware logStdoutDev
    ----- help page
    get (regex "/help|readme") $ do
      readMe <- liftIO $ T.readFile "./README.md"
      html $ sakura <> (TL.fromStrict $ commonmarkToHtml [] readMe)
    ----- IPv4 queries
    get "/ipv4/asn/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ asnipv4diet $ x
    get "/ipv4/country/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ countryipv4diet $ x
    get "/ipv4/city/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ cityBlockipv4diet $ x
    ----- IPv6 queries
    get "/ipv6/asn/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ asnipv6diet $ x
    get "/ipv6/country/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ countryipv6diet $ x
    get "/ipv6/city/:ip" $ do
      query <- param "ip"
      json $ AE.toJSON $ D.lookup query $ cityBlockipv6diet $ x
    ----- gid queries
    get "/gid/city/:gid" $ do
      query <- param "gid"
      json $ AE.toJSON $ MS.lookup (Just query) $ cityLocationMap $ x
    get "/gid/country/:gid" $ do
      query <- param "gid"
      json $ AE.toJSON $ MS.lookup (Just query) $ cityLocationMap $ x

