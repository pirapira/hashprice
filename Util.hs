module Util where

import Import
import Data.Int (Int64)

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
