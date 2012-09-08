
flatten = _.flatten
sortBy = _.sortBy

readReverseLines (lines) ->
  [numChickens, numGoats] = readLine toInts, lines
  chickens = readLines lines, numChickens, toAnimal
  goats = readLines lines, numGoats, toAnimal
  closest = closestPair chickens, goats
  console.log closest.one.id, closest.two.id

toAnimal = toObject ["id", "x", "y"]


closestPair = (chickens, goats) ->
  # you can calculate the distance of one to every other one, and memoize it
  # that's an N^2 problem. 

  # you could treat it as a unidirectional graph and calculate the distances for every starting point?

  distance = (one, two) ->
    dx = Math.abs(one.x - two.x)
    dy = Math.abs(one.y - two.y)
    {distance: Math.sqrt(dx*dx + dy*dy), one, two}

  dss = for chicken in chickens
    ds = for goat in goats
      distance chicken, goat

  flat = flatten dss

  sorted = sortBy flat, (info) -> info.distance
  console.log sorted

  return sorted[0]





