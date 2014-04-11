module Util where

-- use tick
-- use lastBlock

import Import
import Data.Int (Int64)
import Control.Monad (mzero)
import Data.Aeson
import Data.Time.Clock
import Data.Text (unpack)
import Network.HTTP.Types.URI (urlEncode)

import Data.Scientific as Scientific

import qualified Data.Attoparsec.Number as AN
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lex.Lazy.Double (readDouble)
import qualified Data.ByteString as BS
import Network.HTTP.Conduit (simpleHttp)

blockReward :: Block -> Int64
blockReward = reward . blockHeight

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

rawBlockURL :: Hash -> String
rawBlockURL h = "https://blockchain.info/rawblock/" ++ (unpack (unHash h)) -- dangerous

-- urlEncodeString :: String -> String
-- urlEncodeString = BS.unpack . urlEncode True . BS.pack

difficultyURL :: String
difficultyURL = "https://blockchain.info/q/getdifficulty"

tickerJSON :: IO B.ByteString
tickerJSON = simpleHttp tickerURL

lastBlockJSON :: IO B.ByteString
lastBlockJSON = simpleHttp lastBlockURL

rawBlockJSON :: Hash -> IO B.ByteString
rawBlockJSON = simpleHttp . rawBlockURL

difficultyText :: IO B.ByteString
difficultyText = simpleHttp difficultyURL

rawBlockResult :: Hash -> IO (Either String (HM.HashMap Text Value))
rawBlockResult h = eitherDecode <$> rawBlockJSON h

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

fetchTick :: IO (Maybe Tick)
fetchTick = do
  outer <- tickerResult
  case outer of
    Left _ -> return Nothing
    Right out ->
      case HM.lookup "USD" out of
        Nothing -> return Nothing
        Just p -> return $ l2t p

data RawBlock = RawBlock
  { rawBlockHash :: Text
  , rawBlockFee :: Int64
  , rawBlockHeight :: Int64
  , rawBlockTime :: Int64
  }

parseRawBlock :: HM.HashMap Text Value -> Maybe RawBlock
parseRawBlock p = do
  hashV <- look "hash"
  hash <- readText hashV
  feeV <- look "fee"
  fee <- readNumber feeV
  heightV <- look "height"
  height <- readNumber heightV
  timeV <- look "time"
  time <- readNumber timeV
  return $ RawBlock
    { rawBlockHash = unHash hash
    , rawBlockFee = fee
    , rawBlockHeight = height
    , rawBlockTime = time
    }
  where
    look label = HM.lookup label p

fetchRawBlock :: Hash -> IO (Maybe RawBlock)
fetchRawBlock h = do
  outer <- rawBlockResult h
  case outer of
    Left _ -> return Nothing
    Right out ->
      return $ parseRawBlock out

fetchDifficulty :: IO (Maybe Double)
fetchDifficulty = do
   str <- difficultyText
   case readDouble str of
     Nothing -> return Nothing
     Just (d, _) -> return $ Just d

fetchBlock :: Hash -> IO (Maybe Block)
fetchBlock h = do
  current <- getCurrentTime
  fR <- fetchRawBlock h
  fD <- fetchDifficulty
  case (fR, fD) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just rb, Just diff) -> do
      return $ Just
        $ Block
        { blockHash = rawBlockHash rb
        , blockDifficulty = diff
        , blockFee = rawBlockFee rb
        , blockHeight = rawBlockHeight rb
        , blockTime = rawBlockTime rb
        , blockCreatedAt = current
        }

fiveMinAgo :: Handler UTCTime
fiveMinAgo = do
  curTime <- liftIO getCurrentTime
  return $ addUTCTime (5 * 60) curTime

recentBlock :: Handler (Maybe Block)
recentBlock = do
  thr <- fiveMinAgo
  result <- runDB $ selectFirst [BlockCreatedAt >=. thr] [Desc BlockCreatedAt]
  case result of
    Nothing -> return Nothing
    Just (Entity _ b) ->
      return $ Just b

recentTick :: Handler (Maybe Tick)
recentTick = do
  thr <- fiveMinAgo
  result <- runDB $ selectFirst [TickerCreatedAt >=. thr] [Desc TickerCreatedAt]
  case result of
    Nothing -> return Nothing
    Just (Entity _ ticker) ->
      return $ Just $ Tick (tickerUsdbtc ticker)

saveBlock :: Block -> Handler BlockId
saveBlock b = runDB $ insert b

saveTick :: Tick -> Handler TickerId
saveTick t = do
  current <- liftIO getCurrentTime
  runDB $ insert
    $ Ticker
    { tickerUsdbtc = unTick t
    , tickerCreatedAt = current
    }

fetchSaveBlock :: Hash -> Handler Block
fetchSaveBlock h = do
  b <- liftIO $ fetchBlock h
  case b of
    Nothing -> notFound
    Just bl -> do
      _ <- saveBlock bl
      return bl

fetchSaveTick :: Handler Tick
fetchSaveTick = do
  t <- liftIO fetchTick
  case t of
    Nothing -> notFound
    Just tic -> do
      _ <- saveTick tic
      return tic

tick :: Handler Tick
tick = do
  recent <- recentTick
  case recent of
    Just tic -> return tic
    Nothing -> fetchSaveTick

block :: Handler Block
block = do
  recent <- recentBlock
  case recent of
    Just bl -> return bl
    Nothing -> do
      lb <- liftIO lastB
      case lb of
        Nothing -> notFound
        Just h -> fetchSaveBlock h

data Hash = Hash Text deriving (Show, Eq)
unHash :: Hash -> Text
unHash (Hash h) = h

readNumber :: Value -> Maybe Int64
readNumber (Number n) = Just $ round n
readNumber _ = Nothing

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
savelb h = do
  current <- liftIO getCurrentTime
  runDB $ insert
    $ LastBlock
    { lastBlockHash = unHash h
    , lastBlockObtainedAt = current
    }

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

hashBlock :: Hash -> Handler Block
hashBlock h = do
  found <- runDB $ selectFirst [BlockHash ==. unHash h] []
  case found of
    Just (Entity _ b) -> return b
    _ -> getSaveBlock h -- obtain from blockchain.info here

getSaveBlock :: Hash -> Handler Block
getSaveBlock h = do
  b <- liftIO (fetchBlock h)
  case b of
    Nothing -> notFound
    Just bb -> do
      _ <- saveBlock bb
      return bb
