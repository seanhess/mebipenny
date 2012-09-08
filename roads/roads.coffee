
min = _.min
clone = _.clone

readReverseLines (lines) ->
  countries = readCountries lines
  #console.log "COUNTRIES", graphToDijkstraCompatible(countries[0].cities)
  #console.log "CCC", countries
  distances = countries.map countryDistance
  console.log "CHECK", distances
  writeOutputs distances

countryDistance = (country) ->
  distance = shortestDistance country.cities, country.dest, country.coins

readCountries = (lines) ->
  numCountries = readLine toInt, lines
  countries = for [0...numCountries]
    readCountry lines

readCountry = (lines) ->
  coins = readLine toInt, lines
  numCities = readLine toInt, lines
  numRoads = readLine toInt, lines
  roads = readLines lines, numRoads, toRoad
  toCountry coins, numCities, roads

toRoad = (line) ->
  [from, to, weight, toll] = words(line).map(toInt)
  {from, to, weight, toll}

# need to make a graph here
# graph of exits
toCountry = (coins, numCities, roads) ->
  {coins, dest:numCities, cities: toGraph roads}

city = (cities, id) ->
  cities[id] ?= []
  return cities[id]


# gives the shortest possible distance YOU CAN AFFORD
# -1 if you can't afford anything

# TODO: need to make your own tests, with edge cases!


# cities and goalId are the same
# coins is different
# maybe don't discard, just calculate the cost?

# so I want the shortest distance. WITHOUT going over coins!

shortestDistance = (graph, destId, coins) ->
  #console.log "HI", coins

  bellmanFord = (graph, source, coins) ->
    for v in graph.vertices
      if v is source
        v.distance = 0
        v.coins = coins
      else
        v.distance = Infinity
        v.coins = -1

    #console.log "TEST", graph

    for i in [1...graph.vertices.length]
      for uv in graph.edges
        u = graph.vertex uv.from
        v = graph.vertex uv.to
        #console.log "FROM", u.key, "TO", v.key, u.coins, uv.toll
        if u.distance + uv.weight < v.distance and u.coins - uv.toll >= 0
          v.distance = u.distance + uv.weight
          v.coins = u.coins - uv.toll

    for uv in graph.edges
      u = graph.vertex uv.from
      v = graph.vertex uv.to
      if u.distance + uv.weight < v.distance
        return false
        ##throw new Error "Graph contains a negative-weight cycle"

    return graph.vertices

  distances = bellmanFord(graph, graph.vertex(1), coins)

  if not distances then return -1

  dest = graph.vertex destId
  return dest.distance


  #if coins < 0
    #return -1

  #if currentId is goalId
    #return 0

  #paths = []

  #currentCity = city cities, currentId

  #for road in currentCity
    ## source, dest, toll, length
    ## I know the distance here

    #if visited[road.id] then continue
    #visited[road.id] = true

    #nextDistance = shortestDistance(cities, goalId, road.dest, (coins - road.toll), visited)
    #console.log "SHORTEST #{road.id} distance=#{nextDistance}", road.length
    #if nextDistance isnt -1
      #paths.push nextDistance + road.length

  #if not paths.length
    #return -1

  #return min paths

