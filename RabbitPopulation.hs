module RabbitPopulation (progressTime) where

import Data.List (sort)

-- Data

type Age        = Int
type Months     = Int
type GrowthRate = Int

data RabbitPair = RabbitPair Age deriving (Eq, Ord)

instance Show RabbitPair where
   show (RabbitPair age) = 
     if age > 0 then "rr" ++ show age else "bb" ++ show age

-- Operations

makeBabies :: Int -> [RabbitPair]
makeBabies 0         = []
makeBabies numToMake = (RabbitPair 0) : makeBabies (numToMake - 1)

numberAdults :: [RabbitPair] -> Int
numberAdults pairs = length $ filter (\(RabbitPair age) -> age > 0) pairs

ageRabbits :: [RabbitPair] -> [RabbitPair]
ageRabbits = map (\(RabbitPair age) -> RabbitPair (age + 1))

progressTime :: Months -> GrowthRate -> [RabbitPair] -> [RabbitPair]
progressTime 1      growthRate rabbits = rabbits
progressTime months growthRate rabbits =
  progressTime (months - 1) growthRate ((ageRabbits rabbits) ++ newborns)
    where newborns = makeBabies (growthRate * (numberAdults rabbits))

main = do
  print $ progressTime 6 1 [(RabbitPair 0)]
