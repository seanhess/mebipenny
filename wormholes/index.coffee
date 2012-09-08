
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

canGoBackForever = (graph) ->

  console.log "HI", graphToDijkstraCompatible(graph)
  
  goesBack = (vertex, dt) ->
    if vertex.visited?
      if dt < vertex.visited
        return true

      return false

    vertex.visited = dt

    for edge in vertex.edges
      canBack = goesBack (graph.vertex edge.to), (dt + edge.weight)
      if canBack then return true

    return false

  return goesBack (graph.vertex 1), 0


# NO Try an iterative depth-first search?
# Try the algorithm they recommended

# I need to find negative cycles in the graph



