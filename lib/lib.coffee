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
nextLine = curry (parser, reverseLines) ->
  line = reverseLines.pop()
  parser(line)

toInt = (i) -> parseInt i, 10
toString = (i) -> i
toArray = curry (parser, line) -> words(line).map(parser)

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



parseGraphEdge = toObject(["from", "to", "value"])

# builds a uni-directional graph
# expects: [{from: to: value:}]
# returns: {
#   1: [{from: to: value:}]
# }
toGraph = (edges) ->
  graph = {}
  for edge in edges
    graph[edge.from] ?= []
    graph[edge.to] ?= []
    graph[edge.from].push edge
  return graph

# PREP / PARSE DATA and problems, then solve them

words = (string) -> string.split(/\s+/).filter(notEmpty)
lines = (string) -> string.split(/\n/).filter(notEmpty)





