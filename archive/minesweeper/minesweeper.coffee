# N = width
# M = height
# K = number of mines
# lines = coordinates

# OUTPUT
# X, Y, # of mines
# do NOT show any cells that have a count of 0
# do NOT show any cells that have mines on them?
# SORT by X, then Y


# NOTES: I got the size of the spaces wrong. I was including an extra row and column
# I didn't notice this until I wrote "visualize" 
# generally, what happened is that I needed to write my own, more broad inputs to see something was wrong
# but it was hard to visualize. If I had a mine on both corners I would have seen the problem

# SOLUTION: write your own test cases. Have them be simple enough to analyze, but contain as many edge cases as possible




{groupBy} = _

readReverseLines (lines) ->
  [w, h, numMines] = nextLine lines, toArray(toInt)
  mines = nextLines lines, numMines, toXY
  grid = toMineGrid mines
  spaces = allSpaces w, h

  # nonMineSpaces = spaces.filter notHasMine(grid)

  spaces.forEach (space) ->
    space.danger = danger grid, w, h, space.x, space.y

  nonMineSpaces = spaces.filter notHasMine(grid)
  dangerousSpaces = nonMineSpaces.filter hasDanger
  outputs = dangerousSpaces.map spaceToString
  writeOutputs outputs

# PARSING
toXY = (line) ->
  [x, y] = words(line).map(toInt)
  return {x, y}

spaceToString = (space) -> [space.x, space.y, space.danger].join(" ")

# calculates the danger of a given spot
danger = (mineGrid, w, h, x, y) ->
  nearSpaces = nearbyValid(w, h, x, y)
  nearMines = nearSpaces.filter hasMine(mineGrid)
  return nearMines.length


toMineGrid = (mines) ->
  grid = []
  mines.forEach (mine) ->
    key = xykey mine.x, mine.y
    grid[key] = true
  return grid

hasMine = curry (mineGrid, space) ->
  key = xykey space.x, space.y
  return mineGrid[key]?

hasDanger = (space) -> space.danger > 0

notHasMine = curry (mineGrid, space) -> not hasMine(mineGrid, space)

xykey = (x, y) -> "#{x} #{y}"

toMine = (x, y) -> {x, y, type: "mine"}
toSquare = (x, y) ->

# array of all spaces
allSpaces = (w, h) ->
  spaces = []
  for x in [0...w]
    for y in [0...h]
      spaces.push {x, y, danger:0}
  return spaces


nearbyValid = curry (w, h, x, y) ->
  nearby(x, y).filter(validSpace(w, h))

# returns an array of coordinates {x,y}
nearby = (x, y) ->
  spaces = []
  for i in [x-1..x+1]
    for j in [y-1..y+1]
      spaces.push {x:i, y:j}
  return spaces

validSpace = curry (w, h, space) ->
  return (0 <= space.x < w) and (0 <= space.y < h)


visualize = (grid, w, h, spaces) ->
  lastY = 0
  rows = []
  lines = groupBy spaces, (space) -> space.y
  for y, line of lines
    console.log line
    dangers = line.map (space) ->
      if hasMine grid, space then "*"
      else space.danger
    rows.push dangers.join(' ')

  return rows.join('\n')

# you could go through each space, or go through each mine

# I need to calculate how many are adjacent to an given square. to EVERY square
# FILTER out any squares with mines
# FILTER out any squares with zero
# SORT them




