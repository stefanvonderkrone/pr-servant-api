{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Lib
  ( startApp
  , app
  )
where

import           Api
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.List
import           MockData
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.RequestLogger
import           Servant
import           Types

startApp :: IO ()
startApp = do
  regionsTVar <- newTVarIO regions
  hotelsTVar <- newTVarIO hotels
  offersTVar <- newTVarIO offers
  let app' = App { appRegions = regionsTVar
                 , appHotels = hotelsTVar
                 , appOffers = offersTVar
                 , appSetRegions = writeTVar regionsTVar
                 , appSetHotels = writeTVar hotelsTVar
                 , appSetOffers = writeTVar offersTVar
                 }
  run 8080 $ logStdoutDev $ app app'

app :: App -> Application
app app = serve api $ server app

api :: Proxy Api
api = Proxy

-- getters
getAppRegions :: App -> IO [Region]
getAppRegions = readTVarIO . appRegions

getAppHotels :: App -> IO [Hotel]
getAppHotels = readTVarIO . appHotels

getAppOffers :: App -> IO [Offer]
getAppOffers = readTVarIO . appOffers

-- setters
setRegions :: App -> [Region] -> IO ()
setRegions app regions = atomically $ appSetRegions app regions

setHotels :: App -> [Hotel] -> IO ()
setHotels app hotels = atomically $ appSetHotels app hotels

setOffers :: App -> [Offer] -> IO ()
setOffers app offers = atomically $ appSetOffers app offers

-- route-handlers
getRegions :: App -> Handler [Region]
getRegions app = liftIO $ getAppRegions app

putRegion :: App -> Maybe Int -> Maybe String -> Handler Region
putRegion _ Nothing _ = throwError  $ err500 { errBody = "no regionId given" }
putRegion _ _ Nothing = throwError  $ err500 { errBody = "no regionName given" }
putRegion app (Just regionId) (Just regionName) = do
  currentRegions <- liftIO $ getAppRegions app
  let newRegions = currentRegions ++ [region]
  liftIO  $ setRegions app newRegions
  return region
  where
    region = Region regionName regionId

getHotels :: App -> Maybe Int -> Handler [Hotel]
getHotels _ Nothing = throwError $ err500 { errBody = "no regionId given" }
getHotels app (Just regionId) = do
  hotels <- liftIO $ getAppHotels app
  return $ filter ((== regionId) . hotelRegionId) hotels

puthotel :: App -> Maybe Int -> Maybe Int -> Maybe String -> Handler Hotel
puthotel _ Nothing _ _ = throwError $ err500 { errBody = "no regionId given" }
puthotel _ _ Nothing _ = throwError $ err500 { errBody = "no hotelId given" }
puthotel _ _ _ Nothing = throwError $ err500 { errBody = "no hotelName given" }
puthotel app (Just regionId) (Just hotelId) (Just hotelName) = do
  currentHotels <- liftIO $ getAppHotels app
  let newHotels = currentHotels ++ [hotel]
  liftIO  $ setHotels app newHotels
  return hotel
  where
    hotel = Hotel hotelName hotelId regionId

getOffers :: App -> Maybe Int -> Handler [Offer]
getOffers _ Nothing = throwError $ err500 { errBody = "no hotelId given" }
getOffers app (Just hotelId) = do
  offers <- liftIO $ getAppOffers app
  return $ filter ((== hotelId) . offerHotelId) offers

-- our server
server :: App -> Server Api
server app = getRegions app :<|> putRegion app :<|> getHotels app :<|> puthotel app :<|> getOffers app
