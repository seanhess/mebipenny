# requires underscore to be dumped ahead of this file
_ = module.exports


# CURRY makes a function so you can call it with fewer arguments
# it will turn add = (a,b) -> a+b into add = (a) -> (b) -> a+b
# so you can call #2 with add(2)(4)
curry = (f) ->
  call = (args...) ->

    # if we have at least as many arguments as our f supports
    # then call it
    if args.length >= f.length
      f args...

    # otherwise, return a function with the arguments partially applied
    else
      inner = (args2...) ->
        innerArgs = args.concat(args2)
        call innerArgs...
      inner.name = f.name
      inner.toString = -> f.toString()
      inner.curryLength = f.length - args.length # the final length - num of curried arguments
      return inner

  call.name = f.name
  call.toString = -> f.toString()
  call.curryLength = f.length
  return call




writeOutputs = (outputs) ->
  console.log outputs.join("\n")

readStdin = (cb) ->
  data = ""
  process.stdin.resume()
  process.stdin.setEncoding('utf8')
  process.stdin.on 'data', (chunk) ->
    data += chunk.toString()
  process.stdin.on 'end', ->
    cb data


# works for 0, empty strings, etc
notEmpty = (item) -> item

# read as lines, filter empty, and reverse, so functions can use pop
# to access quickly later. Make sure you read them with these parsing functions!!
readReverseLines = (cb) ->
  readStdin (data) ->
    cb lines(data).reverse()

# descructive for performance
nextLine = readLine = curry (parser, reverseLines) ->
  line = reverseLines.pop()
  parser(line)

toInt = (i) -> parseInt i, 10
toString = (i) -> i
toArray = curry (parser, line) -> words(line).map(parser)
toInts = toArray toInt

# convers to integers
toObject = curry (names, line) ->
  values = words(line).map(toInt)
  object = {}
  for i in [0...names.length]
    name = names[i]
    value = values[i]
    object[name] = value
  return object


nextLines = (reverseLines, n, parser) ->
  i = 0
  lines = []
  while i++ < n
    lines.push nextLine parser, reverseLines
  lines

readObjects = (reverseLines, n, linesParser) ->
  objects = []
  for i in [0...n]
    obj = linesParser reverseLines
    objects.push obj
  return objects


# does something n times and returns the results as an array
mapn = (n, f) ->
  objects = []
  for i in [0...n]
    objects.push f()
  return objects



parseGraphEdge = toObject(["from", "to", "weight"])

# builds a uni-directional graph
# expects: [{from: to: weight:}]
# returns: {
#   1: [{from: to: weight:}]
# }

toGraph = (edges) -> return new Graph edges

# PREP / PARSE DATA and problems, then solve them

words = (string) -> string.split(/\s+/).filter(notEmpty)
lines = (string) -> string.split(/\n/).filter(notEmpty)




## GRAPH #############################################

class Graph
  constructor: (@edges = [], @vertices = {}) ->
  vertex: (key) -> @vertices[key]

class Vertex
  constructor: (@key, @edges = []) ->

class Edge
  constructor: (@from, @to, @value) ->


toGraph = (edges) ->
  vertices = {}
  for edge in edges
    vertices[edge.from] ?= new Vertex edge.from
    vertices[edge.to] ?= new Vertex edge.to
    vertices[edge.from].edges.push edge
  return new Graph edges, vertices

# graph = 
#   a: {b: 10, d: 1},
#   b: {a: 1, c: 1, e: 1},

graphToDijkstraCompatible = (graph) ->
  dgraph = {}
  for key, v of graph.vertices
    dgraph[v.key] = dv = {}
    for e in v.edges
      dv[e.to] = e.weight
  return dgraph



###
   arrays
  allVertices: () -> 
    verts = []
    for key, vertex of @vertices
      verts.push vertex
    return verts

###



















Infinity = NaN

# DIJKSTRA - shortest path
# calculates the distance to every vertex in the graph
# dijkstra is faster, but doesn't work for negative edge weights
# basically does breadth first on each node, marking it as visited once all neighbors are checked
# slowly marks things with tentative distances, and replaces them if they get lower
# http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
# https://bitbucket.org/wyatt/dijkstra.js/src/fb6c30a3cebd/dijkstra.js
# see: dijkstra.js




###

# BELLMAN-FORD - shortest path, slower, but works on negative weights
# calculates the distance TO every vertex in the graph

# So I DO need a big list of all the edges
# because the algorithm goes through all the edges each time

# Vertices: just an array of objects. Source must be one of its members

# {from, to, weight}
# from, to = vertex

bellmanFord = (graph, source) ->
 # This implementation takes in a graph, represented as lists of vertices
 # and edges, and modifies the vertices so that their distance and
 # predecessor attributes store the shortest paths.

 # Step 1: initialize graph
 graph.eachVertex (v) ->
   if v is source then v.distance = 0
   else v.distance = Infinity
   v.predecessor = null

 # Step 2: relax edges repeatedly
 for i in [1...graph.count()-1]
   graph.eachEdge (uv) -> # uv is the edge from u to v
     u = graph.vertex uv.from
     v = graph.vertex uv.to
     if u.distance + uv.weight < v.distance
       v.distance = u.distance + uv.weight
       v.predecessor = u

 # Step 3: check for negative-weight cycles
 for uv in edges
   u = uv.from
   v = uv.to
   if u.distance + uv.weight < v.distance
     return new Error "Graph contains a negative-weight cycle"

  return graph
###
