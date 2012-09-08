
min = _.min
clone = _.clone

readReverseLines (lines) ->
  countries = readCountries lines
  distances = countries.map countryDistance
  writeOutputs distances

countryDistance = (country) ->
  distance = shortestDistance country.cities, country.dest, 1, country.coins

readCountries = (lines) ->
  numCountries = nextLine lines, toInt
  countries = for [0...numCountries]
    readCountry lines

readCountry = (lines) ->
  coins = nextLine lines, toInt
  numCities = nextLine lines, toInt
  numRoads = nextLine lines, toInt
  roads = nextLines lines, numRoads, toRoad
  toCountry coins, numCities, roads

toRoad = (line) ->
  [source, dest, length, toll] = words(line).map(toInt)
  {source, dest, length, toll, id: "#{source} -> #{dest}"}

# need to make a graph here
# graph of exits
toCountry = (coins, numCities, roads) ->
  cities = {}

  for road in roads
    source = city cities, road.source
    dest = city cities, road.dest
    source.push road

  {coins, dest:numCities, cities}

city = (cities, id) ->
  cities[id] ?= []
  return cities[id]


# gives the shortest possible distance YOU CAN AFFORD
# -1 if you can't afford anything

# TODO: need to make your own tests, with edge cases!


# cities and goalId are the same
# coins is different
# maybe don't discard, just calculate the cost?

shortestDistance = (cities, goalId, currentId, coins, visited = {}) ->

  console.log "SHORTEST DISTANCE currentId=#{currentId}"

  if coins < 0
    return -1

  if currentId is goalId
    return 0

  paths = []

  currentCity = city cities, currentId

  for road in currentCity
    # source, dest, toll, length
    # I know the distance here

    if visited[road.id] then continue
    visited[road.id] = true

    nextDistance = shortestDistance(cities, goalId, road.dest, (coins - road.toll), visited)
    console.log "SHORTEST #{road.id} distance=#{nextDistance}", road.length
    if nextDistance isnt -1
      paths.push nextDistance + road.length

  if not paths.length
    return -1

  return min paths

