{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Types where

import           Control.Monad                      (liftM)
import           Control.Monad.Reader               (ask)
import           Control.Monad.State                (get, put)
import           Data.Acid
import           Data.Aeson
import           Data.ByteString                    (ByteString)
import           Data.DateTime
import           Data.List
import           Data.SafeCopy
import           Data.Typeable
import           GHC.Generics
import           System.Environment

data TimeSeries = TimeSeries {
  currentHeartRate :: Int
  , timestamp      :: DateTime
} deriving (Generic, Show, Typeable, Eq)

instance Ord TimeSeries where
  compare a b = compare (timestamp a) (timestamp b)

data DBSchema = DBSchema {
  allTimeSeries :: [TimeSeries]
}

$(deriveSafeCopy 0 'base ''TimeSeries)
$(deriveSafeCopy 0 'base ''DBSchema)

instance FromJSON TimeSeries
instance ToJSON TimeSeries

data Credentials = Credentials {
  clientID      :: ByteString
  , clientKey   :: ByteString
  , clientToken :: ByteString
}

addTimeSeries :: TimeSeries -> Update DBSchema ()
addTimeSeries t = do
  DBSchema ts <- get
  put $ DBSchema $ insert t ts

getOrderedTimeSeries :: Query DBSchema [TimeSeries]
getOrderedTimeSeries = do
  db <- ask
  return $ allTimeSeries db

initSchema :: DBSchema
initSchema = DBSchema { allTimeSeries = [] }

makeAcidic ''DBSchema ['addTimeSeries, 'getOrderedTimeSeries]
