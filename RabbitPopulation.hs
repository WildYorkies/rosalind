{-# LANGUAGE BangPatterns #-}
 
module Main where
 
type Age        = Int
type Months     = Int
type GrowthRate = Int
 
type ReproducingRabbits    = Int
type NonReproducingRabbits = Int
 
data RabbitColony = RabbitColony !ReproducingRabbits !NonReproducingRabbits
  deriving (Eq, Ord, Show)
 
progressTime :: Months -> GrowthRate -> RabbitColony -> RabbitColony
progressTime 1 !growthRate rabbits = rabbits
progressTime !months !growthRate (RabbitColony reproducing nonReproducing) =
    progressTime (pred months) growthRate newColony
  where
    newColony = RabbitColony (reproducing+nonReproducing) numberOfBabies
    numberOfBabies = growthRate * reproducing
 
grabResult :: RabbitColony -> Int
grabResult (RabbitColony n _) = n
 
main = do
  print $ grabResult $ progressTime 5 3 (RabbitColony 1 0)
  print $ grabResult $ progressTime 32 5 (RabbitColony 1 0)
