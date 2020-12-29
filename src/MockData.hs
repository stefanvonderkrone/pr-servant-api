module MockData where

import           Types

regions :: [Region]
regions =
  [ Region "Kreta"             0
  , Region "Kos"               1
  , Region "Side & Alanya"     2
  , Region "Hurghada & Safaga" 3
  , Region "Mallorca"          4
  ]

hotels :: [Hotel]
hotels =
      [ Hotel "Anissa Beach & Village"                0  0
      , Hotel "Evangelos Apartments"                  1  0
      , Hotel "Aphrodite Beach Club"                  2  0
      , Hotel "Creta Maris Beach Resort"              3  0
      , Hotel "Pilot Beach Resort"                    4  0
      , Hotel "Sandy Beach Hotel & Family Suites"     5  1
      , Hotel "Sevi Apartments"                       6  1
      , Hotel "LABRANDA Marine Aquapark Resort"       7  1
      , Hotel "Caravia Beach"                         8  1
      , Hotel "Atlantis Hotel"                        9  1
      , Hotel "Side La Grande Resort & Spa"           10 2
      , Hotel "Goldcity Hotel - Villas"               11 2
      , Hotel "Side Crown Palace"                     12 2
      , Hotel "Royal Dragon"                          13 2
      , Hotel "PrimaSol Hane Garden"                  14 2
      , Hotel "Desert Rose Resort"                    15 3
      , Hotel "Albatros Palace Resort"                16 3
      , Hotel "Long Beach Resort"                     17 3
      , Hotel "Beach Albatros Resort"                 18 3
      , Hotel "Dana Beach Resort"                     19 3
      , Hotel "Hipotels Mediterraneo Club"            20 4
      , Hotel "Jade"                                  21 4
      , Hotel "Blau Punta Reina Resort"               22 4
      , Hotel "Aparthotel Eix Platja Daurada - Hotel" 23 4
      , Hotel "Caballero"                             24 4
      ]

offers :: [Offer]
offers = [Offer 84932143298 0 0]
