
min = _.min
find = _.find

readReverseLines (lines) ->
  numCaves = readLine toInt, lines
  numCorridors = nextLine toInt, lines
  corridors = nextLines lines, numCorridors, toCorridor
  console.log corridors
  console.log capacity numCaves, corridors



toCorridor = toObject(["from", "to", "weight"])

# returns an array of capacity by level
# just simulate the dang thing. Put the maximum number of people in each tunnel
# corridors can hold people, and caves can hold people
# dang, this is hard

capacity = (numCaves, corridors) ->
  currentCavePeople = 100000
  for level in [1...numCaves]
    starts = starting corridors, level
    starts.forEach (c) ->
      c.people = c.capacity

starting = (corridors, source) ->
  corridors.filter (c) -> c.source is source

ending = (corridors, dest) ->
  corridors.filter (c) -> c.dest is dest


inPath = (path, [edge, res]) ->
  find path, ([e, r]) -> edge is e and res is r

fordFulkerson = (graph, source, sink) ->

  for uv in graph.edges
    uv.flow = 0

  findPath = (source, sink, path) ->
    if source is sink then return path

    for uv in source.edges
      residual = uv.weight - uv.flow
      if residual > 0 and not inPath path, [edge, residual]
        result = findPath edge.to, sink, path.concat [[edge, residual]]
        if result
          return result
    return null

  path = findPath source, sink, []
  while path
    residuals = 







