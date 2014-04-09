module Util where

import Import
import Data.Int (Int64)
import Control.Monad (mzero)
import Data.Aeson

import Data.Scientific as Scientific
import qualified Data.HashMap.Strict as HM
import qualified Data.Attoparsec.Number as AN
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

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

data Tick = Tick Rational deriving (Show)

instance FromJSON Tick where
  parseJSON (Object v) = Tick <$> v .: "last"
  parseJSON _ = mzero

jsonFile :: FilePath
jsonFile = "./ticker.json"

jsonURL :: String
jsonURL = "https://blockchain.info/ticker"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

tickerResult :: IO (Either String (HM.HashMap Text Value))
tickerResult = (eitherDecode <$> getJSON)

tickL :: IO (Maybe Value)
tickL = do
  outer <- tickerResult
  case outer of
    Left _ -> return $ Nothing
    Right out ->
      return $ HM.lookup "USD" out

l2v :: Value -> Maybe Value
l2v (Object m) = HM.lookup "last" m
l2v _ = Nothing

v2t :: Value -> Maybe Tick
v2t (Number n) = Just $ Tick $ toRational n
v2t _ = Nothing

l2t :: Value -> Maybe Tick
l2t l = l2v l >>= v2t

tick :: IO (Maybe Tick)
tick = do
  prev <- tickL
  case prev of
    Nothing -> return Nothing
    Just p -> return $ l2t p

