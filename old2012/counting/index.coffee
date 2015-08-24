readReverseLines (lines) ->
  numbers = []
  while ((number = readLine toInt, lines))
    numbers.push number
  romans = numbers.map romanize
  writeOutputs romans

