{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Trans.Either
import           Data.Acid                  (openLocalState, query)
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Servant

import           Lib
import           Types

main :: IO ()
main = do
  db <- openLocalState initSchema
  series <- query db GetOrderedTimeSeries
  run 8080 (serve api server)

type Handler a = EitherT ServantErr IO a

type API = "timeseries" :> Get  '[JSON] [TimeSeries]

server :: Server API
server = getTimeSeries

getTimeSeries :: Handler [TimeSeries]
getTimeSeries = do
  return []

api :: Proxy API
api = Proxy
