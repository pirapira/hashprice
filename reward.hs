reward :: Int -> Int
reward height =
  reward_ind height 5000000000

reward_ind :: Int -> Int -> Int
reward_ind height init =
  if height < 210000
  then init
  else reward_ind (height - 210000) $ init `div` 2
