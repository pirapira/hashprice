module Util where

-- use whateverTick

import Import
import Data.Int (Int64)
import Control.Monad (mzero)
import Data.Aeson
import Data.Time.Clock

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

unTick :: Tick -> Rational
unTick (Tick r) = r

instance FromJSON Tick where
  parseJSON (Object v) = Tick <$> v .: "last"
  parseJSON _ = mzero

-- jsonFile :: FilePath
-- jsonFile = "./ticker.json"

tickerURL :: String
tickerURL = "https://blockchain.info/ticker"

lastBlockURL :: String
lastBlockURL = "https://blockchain.info/latestblock"

tickerJSON :: IO B.ByteString
tickerJSON = simpleHttp tickerURL

lastBlockJSON :: IO B.ByteString
lastBlockJSON = simpleHttp lastBlockURL

tickerResult :: IO (Either String (HM.HashMap Text Value))
tickerResult = eitherDecode <$> tickerJSON

lastBlockResult :: IO (Either String (HM.HashMap Text Value))
lastBlockResult = eitherDecode <$> lastBlockJSON


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
  outer <- tickerResult
  case outer of
    Left _ -> return Nothing
    Right out ->
      case HM.lookup "USD" out of
        Nothing -> return Nothing
        Just p -> return $ l2t p

fiveMinAgo :: Handler UTCTime
fiveMinAgo = do
  curTime <- liftIO getCurrentTime
  return $ addUTCTime (5 * 60) curTime

recentTick :: Handler (Maybe Tick)
recentTick = do
  thr <- fiveMinAgo
  result <- runDB $ selectFirst [TickerCreatedAt >=. thr] [Desc TickerCreatedAt]
  case result of
    Nothing -> return $ Nothing
    Just (Entity _ ticker) ->
      return $ Just $ Tick (tickerUsdbtc ticker)

saveTick :: Tick -> Handler TickerId
saveTick t = do
  current <- liftIO getCurrentTime
  runDB $ insert
    $ Ticker
    { tickerUsdbtc = unTick t
    , tickerCreatedAt = current
    }

getSaveTick :: Handler Tick
getSaveTick = do
  t <- liftIO tick
  case t of
    Nothing -> notFound
    Just tic -> do
      _ <- saveTick tic
      return tic

whateverTick :: Handler Tick
whateverTick = do
  recent <- recentTick
  recentJust <- case recent of
              Just tic -> return tic
              Nothing -> getSaveTick
  return recentJust

data Hash = Hash Text deriving (Show, Eq)
unHash :: Hash -> Text
unHash (Hash h) = h

readText :: Value -> Maybe Hash
readText (String t) = Just $ Hash t
readText _ = Nothing

lastB :: IO (Maybe Hash)
lastB = do
  outer <- lastBlockResult
  case outer of
    Left _ -> return Nothing
    Right out ->
      case HM.lookup "hash" out of
        Nothing -> return Nothing
        Just p -> return $ readText p

savelb :: Hash -> Handler LastBlockId
savelb = undefined

getSaveLastBlock :: Handler Hash
getSaveLastBlock = do
  l <- liftIO lastB
  case l of
    Nothing -> notFound
    Just lb -> do
      _ <- savelb lb
      return lb

lastBlock :: Handler Hash
lastBlock = do
  thr <- fiveMinAgo
  found <- runDB $ selectFirst [LastBlockObtainedAt >=. thr] [Desc LastBlockObtainedAt]
  case found of
    Just (Entity _ lb) -> return $ Hash $ lastBlockHash lb
    Nothing -> getSaveLastBlock

block :: Hash -> Handler Block
block h = do
  found <- runDB $ selectFirst [BlockHash ==. unHash h] []
  case found of
    Just (Entity _ b) -> return b
    _ -> getSaveBlock h -- obtain from blockchain.info here

getSaveBlock :: Hash -> Handler Block
getSaveBlock h = undefined
