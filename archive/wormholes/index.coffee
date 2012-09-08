
clone = _.clone

# It's too slow (roads worked too, with the clone thing, but it dies)
# 3 correct, and 3 timed out... hmmm. 

readReverseLines (lines) ->

  [numUniverses] = readLine toInts, lines
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

# IN GENERAL: implement your recursions like this: so you can memoize internal stuff if you want
# except that's really bad, since visited is global...

###
canGoBackForever = (graph) ->

  goesBack = (vertex, dt, visited = {}) ->
    if visited[vertex.key]?
      if dt < visited[vertex.key]
        return true

      return false

    visited[vertex.key] = dt

    for edge in vertex.edges
      canBack = goesBack (graph.vertex edge.to), (dt + edge.weight), visited
      if canBack then return true

    return false

  return goesBack (graph.vertex 1), 0
###

# gets 6/6 (well, I know how to detect negative cycles at least
canGoBackForever = (graph) ->
  hasNegativeCycle = not bellmanFord graph, graph.vertex(1)
  return hasNegativeCycle

# NO Try an iterative depth-first search?
# Try the algorithm they recommended

# I need to find negative cycles in the graph



