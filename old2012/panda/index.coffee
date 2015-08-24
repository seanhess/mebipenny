
sortBy = _.sortBy
without = _.without
max = _.max

readReverseLines (lines) ->
  clouds = []
  while ((cloud = readLine toCloud, lines))
    clouds.push cloud
  best = clouds.map bestSolution
  outs = best.map toOutput
  writeOutputs outs

toOutput = (solution) ->
  if not solution? then "NO SOLUTION"
  else solution.join " "

toCloud = (line) ->
  if not line then return false
  ints = words(line).map(toInt)
  {capacity: ints[0], empress: ints[1], guards: ints.slice(2)}

# return
bestSolution = (cloud) ->
  all = solutions (cloud.capacity - cloud.empress), cloud.guards

  if not all? then return null

  # they must be from lightest to heaviest
  sortedGuards = all.map (solution) ->
    solution.sort()

  best = sortBy sortedGuards, (solution) ->
    #solution[solution.length-1]
    solution.length

  if best.length is 0 then return []

  goodLength = best[best.length-1].length

  onlyBestLengths = best.filter (solution) -> solution.length is goodLength

  if onlyBestLengths.length > 1
    best = sortBy onlyBestLengths, (solution) ->
      solution[solution.length-1]

  return best[best.length-1]

# returns all solutions
# [[guard]]
solutions = (capacity, guards) ->

  #console.log "SOLUTIONS", capacity, guards
  if capacity is 0 then return []
  if capacity < 0 then return null

  if guards.length is 0 then return null

  matches = []

  for guard in guards
    remaining = capacity - guard
    #console.log " - checking guard: #{guard} remaining: #{remaining}"
    if remaining is 0
      #console.log "   found"
      matches.push [guard]
    if remaining < 0
      #console.log "   skip"
      continue
    if remaining > 0
      #console.log "   subs"
      subs = solutions remaining, without(guards, guard)
      if subs
        for sub in subs
          matches.push [guard].concat sub

  #console.log "FOUND", capacity, matches

  return matches

# SOLUTION = [] of guard weights

