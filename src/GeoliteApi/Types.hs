{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE UnboxedTuples         #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module GeoliteApi.Types
  ( ASN(..)
  , Country(..)
  , CountryLocation(..)
  , CityBlock(..)
  , CityLocation(..)
  , FatBool(..)
  , MaybeInt(..), boxMaybeInt, unboxMaybeInt
  , MaybeDouble(..), boxMaybeDouble, unboxMaybeDouble 
  , Maps(..)
  ) where

import           Control.DeepSeq                     ( NFData (rnf) )
import qualified Country                             as Country
import           Data.Aeson                          ( ToJSON, object, toJSON, (.=) )
import qualified Data.Aeson                          as AE
import qualified Data.Diet.Map.Strict.Unboxed.Lifted as D
import qualified Data.Map.Strict                     as MS
import qualified Data.Text.Lazy                      as L
import           Data.Text.Short                     ( ShortText )
import qualified Data.Text.Short                     as TS
import           GHC.Exts                            ( Int#, Double#, (==#), (==##), Double (D#), Int (I#) )
import           GHC.Generics                        ( Generic )
import           Net.IPv4                            ( IPv4 )
import qualified Net.IPv4                            as IPv4
import           Net.IPv6                            ( IPv6 )
import qualified Net.IPv6                            as IPv6
import           Web.Scotty

-- | Type of ASN csv
data ASN = ASN
  { autonomous_system_number       :: Maybe Int
  , autonomous_system_organization :: ShortText
  }
  deriving(Show, Eq, Generic, NFData)

-- | Type of Country csvs
data Country = Country
  { geoname_id                     :: Maybe Int
  , registered_country_geoname_id  :: Maybe Int
  , represented_country_geoname_id :: Maybe Int
  , is_anonymous_proxy             :: FatBool
  , is_satellite_provider          :: FatBool
  }
  deriving(Show, Eq, Generic, NFData)

-- | Type of Country Location csv
data CountryLocation = CountryLocation
  { locale_code          :: ShortText
  , continent_code       :: ShortText
  , continent_name       :: ShortText
  , country_iso_code     :: Country.Country
  , country_name         :: Country.Country
  , is_in_european_union :: FatBool
  }
  deriving(Show, Eq, Generic, NFData)

-- | Type of CityBlock csv
data CityBlock = CityBlock
  { cityBlockGeonameId             :: MaybeInt
  , registered_country_geoname_id  :: MaybeInt
  , represented_country_geoname_id :: MaybeInt
  , is_anonymous_proxy             :: FatBool
  , is_satellite_provider          :: FatBool
  , postal_code                    :: ShortText
  , latitude                       :: MaybeDouble
  , longitude                      :: MaybeDouble
  , accuracy_radius                :: MaybeInt
  }
  deriving(Show, Eq, Generic, NFData)

-- | A 'MaybeInt' is isomorphic to 'Maybe' 'Int',
--   but allows the 'Int' inside to unbox.
data MaybeInt = MaybeInt (# (# #) | Int# #)

instance Show MaybeInt where
  showsPrec p m = showsPrec p (boxMaybeInt m)

instance Eq MaybeInt where
  a == b = boxMaybeInt a == boxMaybeInt b 
--  MaybeInt a == MaybeInt b = case a of
--    (# (# #) | #) -> case b of
--      (# (# #) | #) -> True
--      _             -> False
--    (# | a_i# #) -> case b of
--      (# (# #) | #) -> False 
--      (# | b_i# #)  -> case a_i# ==# b_i# of
--        1# -> True
--        _  -> False
--  {-# INLINE (==) #-}

-- why does the box the 'MaybeInt' instead of
-- just returning '()'?
instance NFData MaybeInt where
  rnf mi = rnf $ boxMaybeInt mi
  {-# INLINE rnf #-}

-- | Convert a 'MaybeInt' to a 'Maybe' 'Int'.
boxMaybeInt :: MaybeInt -> Maybe Int
boxMaybeInt (MaybeInt x) = case x of
  (# (# #) | #) -> Nothing
  (# | i #)     -> Just (I# i)

-- | Convert a 'Maybe' 'Int' to a 'MaybeInt'.
unboxMaybeInt :: Maybe Int -> MaybeInt
unboxMaybeInt = \case
  Nothing -> MaybeInt (# (# #) | #)
  Just (I# i) -> MaybeInt (# | i #)

<<<<<<< HEAD
data MaybeDouble = MaybeDouble (# (# #) | Double# #)

instance Eq MaybeDouble where
  a == b = boxMaybeDouble a == boxMaybeDouble b

--  MaybeDouble a == MaybeDouble b = case a of
--    (# (# #) | #) -> case b of
--      (# (# #) | #) -> True
--      _             -> False
--    (# | a_i# #) -> case b of
--      (# (# #) | #) -> False 
--      (# | b_i# #)  -> case a_i# ==## b_i# of
--        1# -> True
--        _  -> False
--  {-# INLINE (==) #-}

instance Show MaybeDouble where
  showsPrec p m = showsPrec p (boxMaybeDouble m)

instance NFData MaybeDouble where
  rnf md = rnf $ boxMaybeDouble md
  {-# INLINE rnf #-}

boxMaybeDouble :: MaybeDouble -> Maybe Double
boxMaybeDouble (MaybeDouble x) = case x of
  (# (# #) | #) -> Nothing
  (# | d #)     -> Just (D# d)

unboxMaybeDouble :: Maybe Double -> MaybeDouble
unboxMaybeDouble = \case
  Nothing -> MaybeDouble (# (# #) | #)
  Just (D# d) -> MaybeDouble (# | d #)

-- | Type of City Location csv
data CityLocation = CityLocation
  { locale_code            :: ShortText
  , continent_code         :: ShortText
  , continent_name         :: ShortText
  , country_iso_code       :: Country.Country
  , country_name           :: Country.Country
  , subdivision_1_iso_code :: ShortText
  , subdivision_1_name     :: ShortText
  , subdivision_2_iso_code :: ShortText
  , subdivision_2_name     :: ShortText
  , city_name              :: ShortText
  , metro_code             :: ShortText
  , time_zone              :: ShortText
  , is_in_european_union   :: FatBool
  }
  deriving(Show, Eq, Generic, NFData)

-- | Type representing maps of each CSV
data Maps = Maps
  { asnipv4diet        :: D.Map IPv4 ASN
  , asnipv6diet        :: D.Map IPv6 ASN
  , countryipv4diet    :: D.Map IPv4 Country
  , countryipv6diet    :: D.Map IPv6 Country
  , cityBlockipv4diet  :: D.Map IPv4 CityBlock
  , cityBlockipv6diet  :: D.Map IPv6 CityBlock
  , cityLocationMap    :: MS.Map (Maybe Int) CityLocation
  , countryLocationMap :: MS.Map (Maybe Int) CountryLocation
  }

-- | Similar to 'Bool', but fuzzy - things can be
--   True, False, or neither True nor False.
data FatBool = FatTrue | FatFalse | NotTrueOrFalse
  deriving(Show, Eq, Generic, NFData)

instance Parsable IPv4 where
  parseParam ip = case IPv4.decode $ L.toStrict ip of
    Nothing -> Left "parseParam IPv4: no parse"
    Just i  -> Right i

deriving instance NFData  IPv4
deriving instance NFData  IPv6
deriving instance Generic IPv6

instance Parsable IPv6 where
  parseParam ip = case IPv6.decode $ L.toStrict ip of
    Nothing -> Left "parseParam IPv6: no parse"
    Just i  -> Right i

instance ToJSON FatBool where
  toJSON FatTrue        = AE.Bool True
  toJSON FatFalse       = AE.Bool False
  toJSON NotTrueOrFalse = AE.Null

instance ToJSON ShortText where
  toJSON t = AE.String $ TS.toText t

instance ToJSON ASN where
  toJSON (ASN a b) = object
    [ "autonomous_system_number"       .= fmap intNull a
    , "autonomous_system_organization" .= b
    ]

instance ToJSON Country where
  toJSON (Country a b c d e) = object
    [ "geoname_id"                     .= fmap intNull a
    , "registered_country_geoname_id"  .= fmap intNull b
    , "represented_country_geoname_id" .= fmap intNull c
    , "is_anonymous_proxy"             .= d
    , "is_satellite_provider"          .= e
    ]

instance ToJSON CountryLocation where
  toJSON (CountryLocation a b c d e f)  = object
    [ "locale_code"                    .= textNull a
    , "lcontinent_code"                .= textNull b
    , "lcontinent_name"                .= textNull c
    , "lcountry_iso_code"              .= Country.alphaTwoUpper d
    , "lcountry_name"                  .= e
    , "lis_in_european_union"          .= f
    ]

instance ToJSON CityBlock where
  toJSON (CityBlock a b c d e f g h i) = object
    [ "cityBlockGeonameId"             .= fmap intNull (boxMaybeInt a)
    , "registered_country_geoname_id"  .= fmap intNull (boxMaybeInt b)
    , "represented_country_geoname_id" .= fmap intNull (boxMaybeInt c)
    , "is_anonymous_proxy"             .= d
    , "is_satellite_provider"          .= e
    , "postal_code"                    .= textNull f
    , "latitude"                       .= fmap doubleNull (boxMaybeDouble g)
    , "longitude"                      .= fmap doubleNull (boxMaybeDouble h)
    , "accuracy_radius"                .= fmap intNull (boxMaybeInt i)
    ]

instance ToJSON CityLocation where
  toJSON (CityLocation a b c d e f g h i j k l m) = object
    [ "locale_code"            .= textNull a
    , "continent_code"         .= textNull b
    , "continent_name"         .= textNull c
    , "country_iso_code"       .= Country.alphaTwoUpper d
    , "country_name"           .= e
    , "subdivision_1_iso_code" .= textNull f
    , "subdivision_1_name"     .= textNull g
    , "subdivision_2_iso_code" .= textNull h
    , "subdivision_2_name"     .= textNull i
    , "city_name"              .= textNull j
    , "metro_code"             .= textNull k
    , "time_zone"              .= textNull l
    , "is_in_european_union"   .= m
    ]

-- | Fixme: doc
textNull :: ShortText -> AE.Value
textNull "" = AE.Null
textNull a  = AE.String $ TS.toText a

-- | Fixme: doc
intNull :: Int -> AE.Value
intNull 0 = AE.Null
intNull x = toJSON x

doubleNull :: Double -> AE.Value
doubleNull 0 = AE.Null
doubleNull x = toJSON x
