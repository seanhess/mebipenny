
clone = _.clone

# It's too slow (roads worked too, with the clone thing, but it dies)
# 3 correct, and 3 timed out... hmmm. 

readReverseLines (lines) ->
  numUniverses = nextLine toInt, lines
  universes = readObjects lines, numUniverses, readUniverse
  results = universes.map (universe) ->
    canGoBackForever universe.graph
  yn = results.map (bool) -> if bool then "Y" else "N"
  writeOutputs yn

readUniverse = (lines) ->
  [numGalaxies, numWormholes] = nextLine toArray(toInt), lines
  wormholes = readObjects lines, numWormholes, nextLine(parseGraphEdge)
  graph = toGraph wormholes
  return {numGalaxies, graph}


# can you make the call NOT dependant on dt? ... 
# with an accumulator? that's what it is


# This method gets 4/6 without timing out
# cloning gets 3/6 with 3 timeouts
canGoBackForever = (graph, currentNode = 1, dt = 0, visited = {}) ->

  if visited[currentNode]
    if dt < visited[currentNode]
      return true

    return false

  visited[currentNode] = dt

  for edge in graph[currentNode]
    canBack = canGoBackForever graph, edge.to, (dt + edge.value), visited
    if canBack then return true

  return false





# Try an iterative depth-first search?
# Try the algorithm they recommended



# how do you prevent loops? (with memoize!)
# if your calculated value is different from the memoized version?
# you don't even GET a value for that node though

# Need to detect negative cycles


# Break graph into strongly connected components

amountBackInTime = (graph, currentNode = 1) ->

  for edge in graph[currentNode]
    subDt = amountBackInTime graph, edge.to
    totalDt = edge.value


