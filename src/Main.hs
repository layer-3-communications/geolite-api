{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Colonnade                    ( Headed )
import           Control.Monad                ( guard, (<=<) )
import           GeoliteApi.Types
import           Net.IPv4                     ( IPv4 )
import           Net.IPv6                     ( IPv6 )
import           Web.Scotty
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Streaming    as BS
import qualified Data.Diet.Map.Unboxed.Lifted as D
import qualified Data.Map.Strict              as MS
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Net.IPv4                     as IPv4
import qualified Net.IPv6                     as IPv6
import qualified Siphon                       as SI
import qualified Streaming                    as SR
import qualified Streaming.Prelude            as SR
import qualified System.INotify               as IN
import qualified System.IO                    as IO

-- functions used by siphon to decode csvs
siphonAsn :: SI.Siphon Headed B.ByteString (IPv4, IPv4, ASN)
siphonAsn = (\r asn -> (IPv4.lowerInclusive r,IPv4.upperInclusive r,asn))
  <$> SI.headed "network" ipv4Decode
  <*> ( ASN
        <$> SI.headed "autonomous_system_number" numDecode
        <*> SI.headed "autonomous_system_organization" textDecode
      )

siphonAsnv6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, ASNv6)
siphonAsnv6 = (\r asn -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,asn))
  <$> SI.headed "network" ipv6Decode
  <*> ( ASNv6
        <$> SI.headed "autonomous_system_number" numDecode
        <*> SI.headed "autonomous_system_organization" textDecode
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

siphonCountryV6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, CountryV6)
siphonCountryV6 = (\r c -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,c))
  <$> SI.headed "network" ipv6Decode
  <*> ( CountryV6
        <$> SI.headed "geoname_id" numDecode
        <*> SI.headed "registered_country_geoname_id" numDecode
        <*> SI.headed "represented_country_geoname_id" numDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
      )

siphonCityBlock :: SI.Siphon Headed B.ByteString (IPv4, IPv4, CityBlock)
siphonCityBlock = (\r cb -> (IPv4.lowerInclusive r,IPv4.upperInclusive r,cb))
  <$> SI.headed "network" ipv4Decode
  <*> ( CityBlock
        <$> SI.headed "geoname_id" numDecode
        <*> SI.headed "registered_country_geoname_id" numDecode
        <*> SI.headed "represented_country_geoname_id" numDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
        <*> SI.headed "postal_code" textDecode
        <*> SI.headed "latitude" numDecode
        <*> SI.headed "longitude" numDecode
        <*> SI.headed "accuracy_radius" numDecode
      )

siphonCityBlockV6 :: SI.Siphon Headed B.ByteString (IPv6, IPv6, CityBlock)
siphonCityBlockV6 = (\r cb -> (IPv6.lowerInclusive r,IPv6.upperInclusive r,cb))
  <$> SI.headed "network" ipv6Decode
  <*> ( CityBlock
        <$> SI.headed "geoname_id" numDecode
        <*> SI.headed "registered_country_geoname_id" numDecode
        <*> SI.headed "represented_country_geoname_id" numDecode
        <*> SI.headed "is_anonymous_proxy" boolDecode
        <*> SI.headed "is_satellite_provider" boolDecode
        <*> SI.headed "postal_code" textDecode
        <*> SI.headed "latitude" numDecode
        <*> SI.headed "longitude" numDecode
        <*> SI.headed "accuracy_radius" numDecode
      )

siphonCityLocations :: SI.Siphon Headed B.ByteString (Int, CityLocation)
siphonCityLocations = (\g cl -> (g,cl))
  <$> SI.headed "geoname_id" numDecode
  <*> ( CityLocation
        <$> SI.headed "locale_code" textDecode
        <*> SI.headed "continent_code" textDecode
        <*> SI.headed "continent_name" textDecode
        <*> SI.headed "country_iso_code" textDecode
        <*> SI.headed "country_name" textDecode
        <*> SI.headed "subdivision_1_iso_code" textDecode
        <*> SI.headed "subdivision_1_name" textDecode
        <*> SI.headed "subdivision_2_iso_code" textDecode
        <*> SI.headed "subdivision_2_name" textDecode
        <*> SI.headed "city_name" textDecode
        <*> SI.headed "metro_code" textDecode
        <*> SI.headed "time_zone" textDecode
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

textDecode :: B.ByteString -> Maybe T.Text
textDecode = either (const Nothing) Just . TE.decodeUtf8'
numDecode :: B.ByteString -> Maybe Int
numDecode  = readIntExactly
boolDecode :: B.ByteString -> Maybe FatBool
boolDecode = fmap intToBool . readIntExactly
ipv4Decode :: B.ByteString -> Maybe IPv4.IPv4Range
ipv4Decode = IPv4.decodeRange <=< either (const Nothing) Just . TE.decodeUtf8'
ipv6Decode :: B.ByteString -> Maybe IPv6.IPv6Range
ipv6Decode = IPv6.decodeRange <=< either (const Nothing) Just . TE.decodeUtf8'

mkBlock ::
     Show a =>
     FilePath
  -> SI.Siphon Headed B.ByteString a
  -> IO (SR.Of [a] (Maybe SI.SiphonError))
mkBlock path s = IO.withFile path IO.ReadMode
  (\handle -> SR.toList $
      SI.decodeCsvUtf8 s (BS.toChunks $ BS.fromHandle handle))

-- filepaths of csv
asnipv4path :: FilePath
asnipv4path = "./geolite2/GeoLite2-ASN-CSV_20180709/GeoLite2-ASN-Blocks-IPv4.csv"
asnipv6path :: FilePath
asnipv6path = "./geolite2/GeoLite2-ASN-CSV_20180709/GeoLite2-ASN-Blocks-IPv6.csv"
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

-----------------------
main :: IO ()
main = do
  -- binds the decoded csv
  asnipv4@       ( asnls SR.:> _          ) <- mkBlock asnipv4path       siphonAsn
  asnipv6@       ( asnv6ls SR.:> _        ) <- mkBlock asnipv6path       siphonAsnv6
  countryipv4@   ( countryls SR.:> _      ) <- mkBlock countryipv4path   siphonCountry
  countryipv6@   ( countryv6ls SR.:> _    ) <- mkBlock countryipv6path   siphonCountryV6
  cityBlockipv4@ ( cityBlockls SR.:> _    ) <- mkBlock cityBlockipv4path siphonCityBlock
  cityBlockipv6@ ( cityBlockv6ls SR.:> _  ) <- mkBlock cityBlockipv6path siphonCityBlockV6
  cityLocation@  ( cityLocationls SR.:> _ ) <- mkBlock cityLocationpath  siphonCityLocations

  -- converts the csv to a diet map
  let asnipv4diet       = D.fromList  asnls
      asnipv6diet       = D.fromList  asnv6ls
      countryipv4diet   = D.fromList  countryls
      countryipv6diet   = D.fromList  countryv6ls
      cityBlockipv4diet = D.fromList  cityBlockls
      cityBlockipv6diet = D.fromList  cityBlockv6ls
      cityLocationMap   = MS.fromList cityLocationls

  pure ()
