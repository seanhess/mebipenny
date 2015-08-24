max = _.max

readReverseLines (lines) ->
  [degrees] = readInts lines
  strings = []
  while ((string = readString lines))
    strings.push string
  maxstring = max strings, (s) -> s.length
  padded = strings.map padSpace(maxstring.length)
  arrays = padded.map toArray
  rotated = rotate degrees, arrays
  ls = rotated.map toLineString
  writeOutputs ls
  #console.log arrays

toLineString = (line) -> line.join ""

padSpace = curry (max, string) ->
  while string.length < max
    string = string + " "
  return string

toArray = (string) ->
  arr = []
  for c in string
    arr.push c
  return arr

rotate = (degrees, arrays) ->

  clockwiseRotations = (degrees / 90) % 4
  #counterRotations = 4-clockwiseRotations

  if clockwiseRotations < 0
    clockwiseRotations = 4 + clockwiseRotations

  while clockwiseRotations-- > 0
    arrays = rotateOnce arrays
    #console.log arrays

  return arrays


# NOT A ROTATE! It's a flip over XY axis
rotateOnce = (arrays) ->
  dest = []

  max = arrays.length-1
  for y in [0...arrays[0].length]
    row = arrays[y]
    for x in [0...arrays.length]
      ox = y
      oy = max - x
      dest[y] ?= []
      dest[y][x] = arrays[oy][ox]

  return dest

#console.log "WOOT", $M

# you just change the coordinate system
#newPosition = (rotations, x, y) ->
  #rotates = rotations % 4


