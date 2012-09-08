
readReverseLines (lines) ->
  numInputs = nextLine lines, toInt
  inputs = nextLines lines, numInputs, toArray(toInt)
  outputs = inputs.map ([a, b]) ->
    uniqueFibsBetween a, b
  writeOutputs outputs

fibonacci = memoize (n) ->
  if n < 2 then n
  else fibonacci(n-1) + fibonacci(n-2)

uniqueFibsBetween = (a, b) ->
  i = 2 # start at 2 to avoid dups with 1
  num = 0
  if a is 0 then num++
  while fibonacci(i) <= b
    if fibonacci(i) >= a then num++
    i++
  num


