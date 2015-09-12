-- M countries
-- each: N cities 1..N
-- connected with 1 way roads
-- each road has 

import Prelude
import Data.Vector

-- roads are my edges
data Road = Road
  { source :: Int
  , dest :: Int
  , length :: Int
  , toll :: Int
  } deriving (Show, Eq)

data City = City
  { id :: Int
  , roads :: [Road]
  }

type Country = Vector City

-- roads are all one way!
-- well, who cares?
-- SKIP ME! is the correct answer here!
-- 

main = putStrLn "Hello"
