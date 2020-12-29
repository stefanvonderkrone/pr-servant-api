{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  )
where

import           Servant
import           Types

-- GET /region
type GetRegions = "region" :> Get '[JSON] [Region]
-- PUT /region?regionId={Int}&regionName={String}
type PutRegion = "region" :> QueryParam "regionId" Int :> QueryParam "regionName" String :> Put '[JSON] Region

-- GET /hotel?regionId={Int}
type GetHotels = "hotel" :> QueryParam "regionId" Int :> Get '[JSON] [Hotel]
-- PUT /hotel?regionId={Int}&hotelId={Int}&hotelName={String}
type PutHotel = "hotel" :> QueryParam "regionId" Int :> QueryParam "hotelId" Int :> QueryParam "hotelName" String :> Put '[JSON] Hotel

-- GET /angebot?hotelId={Int}
type GetOffers = "angebot" :> QueryParam "hotelId" Int :> Get '[JSON] [Offer]

-- -- GET /dateneingabe/:bookingId
-- type GetBooking = "dateneingabe" :> Capture "bookingId" String

-- /suche/{region,hotel,angebot}
type Api = "suche" :> (GetRegions :<|> PutRegion :<|> GetHotels :<|> PutHotel :<|> GetOffers)
