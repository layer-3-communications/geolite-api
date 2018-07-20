{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}

module GeoliteApi.Types where

import Data.Text ( Text )

-- data types representing the decoded information in the CSVs
data ASN = ASN
  { autonomous_system_number       :: Int
  , autonomous_system_organization :: Text
  }
  deriving(Show, Eq)

data ASNv6 = ASNv6
  { autonomous_system_number       :: Int
  , autonomous_system_organization :: Text
  }
  deriving(Show, Eq)

data Country = Country
  { geoname_id                     :: Int
  , registered_country_geoname_id  :: Int
  , represented_country_geoname_id :: Int
  , is_anonymous_proxy             :: FatBool
  , is_satellite_provider          :: FatBool
  }
  deriving(Show, Eq)

data CountryV6 = CountryV6
  { geoname_id                     :: Int
  , registered_country_geoname_id  :: Int
  , represented_country_geoname_id :: Int
  , is_anonymous_proxy             :: FatBool
  , is_satellite_provider          :: FatBool
  }
  deriving(Show, Eq)

data CityBlock = CityBlock
  { cityBlockGeonameId             :: Int
  , registered_country_geoname_id  :: Int
  , represented_country_geoname_id :: Int
  , is_anonymous_proxy             :: FatBool
  , is_satellite_provider          :: FatBool
  , postal_code                    :: Text
  , latitude                       :: Int
  , longitude                      :: Int
  , accuracy_radius                :: Int
  }
  deriving(Show, Eq)

data CityLocation = CityLocation
  { locale_code            :: Text
  , continent_code         :: Text
  , continent_name         :: Text
  , country_iso_code       :: Text
  , country_name           :: Text
  , subdivision_1_iso_code :: Text
  , subdivision_1_name     :: Text
  , subdivision_2_iso_code :: Text
  , subdivision_2_name     :: Text
  , city_name              :: Text
  , metro_code             :: Text
  , time_zone              :: Text
  , is_in_european_union   :: FatBool
  }
  deriving(Show, Eq)

data FatBool = FatTrue | FatFalse | NotTrueOrFalse
  deriving(Show, Eq)

