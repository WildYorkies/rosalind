module RabbitPopulation (progressTime) where

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

growPop :: GrowthRate -> [RabbitPair] -> [RabbitPair]
growPop growthRate pairs = 
  (makeBabies (growthRate * (numberAdults pairs)))

progressTime :: Months -> GrowthRate -> [RabbitPair] -> [RabbitPair]
progressTime 1      growthRate rabbits = rabbits
progressTime months growthRate rabbits =
  progressTime (months - 1) growthRate ((ageRabbits rabbits) ++ newborns)
    where newborns = growPop growthRate rabbits

main = do
  print $ progressTime 6 1 [(RabbitPair 0)] 
  -- Ouput matches the image here: https://cdn-images-1.medium.com/max/600/1*S8NqTJkpoIFrXH4gzyrDsQ.png
