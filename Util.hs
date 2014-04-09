module Util where

import Import
import Data.Int (Int64)
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as B

blockReward :: Block -> Int64
blockReward block =
  reward $ blockHeight block

reward :: Int64 -> Int64
reward height =
  reward_ind height 5000000000

reward_ind :: Int64 -> Int64 -> Int64
reward_ind height init =
  if height < 210000
  then init
  else reward_ind (height - 210000) $ init `div` 2

data TickResult = TickResult !Object deriving (Show)

instance FromJSON TickResult where
  parseJSON (Object v) = TickResult <$> v .: "USD"
  parseJSON _ = mzero

data Tick = Tick Text deriving (Show)

instance FromJSON Tick where
  parseJSON (Object v) = Tick <$> v .: "last"
  parseJSON _ = mzero

jsonFile :: FilePath
jsonFile = "./ticker.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

tickerResult :: IO (Either String (HM.HashMap Text Text))
tickerResult = (eitherDecode <$> getJSON)

tick :: IO (Maybe Tick)
tick = do
  outer <- tickerResult
  case outer of
    Left _ -> return $ Nothing
    Right out ->
      return $ fmap Tick $ HM.lookup "last" out
