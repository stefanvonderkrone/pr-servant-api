{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Types
  ( Region(..)
  , Hotel(..)
  , Offer(..)
  , App(..)
  )
where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Text.Casing

data Region = Region { regionName :: String, regionId :: Int } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = camel . drop 6 } ''Region)

data Hotel = Hotel { hotelName :: String, hotelId :: Int, hotelRegionId :: Int } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = camel. drop 5 } ''Hotel)

data Offer = Offer { offerId :: Int, offerTourOperatorId :: Int, offerHotelId :: Int } deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = camel . drop 5 } ''Offer)

data App = App
  { appRegions    :: TVar [Region]
  , appHotels     :: TVar [Hotel]
  , appOffers     :: TVar [Offer]
  , appSetRegions :: [Region] -> STM ()
  , appSetHotels  :: [Hotel] -> STM ()
  , appSetOffers  :: [Offer] -> STM ()
  }
