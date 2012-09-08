
readReverseLines (lines) ->
  codes = []
  while (string = nextLine(lines, toString)) isnt "0"
    codes.push string

  possibilities = codes.map totalPossibleMessages

  writeOutputs possibilities




# need to keep a tree of possible combinations
# what are the ambiguous entries?
# z i 1A... yay

###
look at the first 2. Is it combo? is the first one a combo?
  - both: need to branch
  - just one: pop that many off and go
###

codemap =
 "1" :"a"
 "2" :"b"
 "3" :"c"
 "4" :"d"
 "5" :"e"
 "6" :"f"
 "7" :"g"
 "8" :"h"
 "9" :"i"
 "A" :"j"
 "B" :"k"
 "C" :"l"
 "D" :"m"
 "E" :"n"
 "F" :"o"
 "10":"p"
 "11":"q"
 "12":"r"
 "13":"s"
 "14":"t"
 "15":"u"
 "16":"v"
 "17":"w"
 "18":"x"
 "19":"y"
 "1A":"z"
                      

exists = (combo) ->
  codemap[combo]?


### WHAT DID I LEARN?

I needed to unroll the recursion to understand what I wanted to do
I made many mistakes
I caught all my problems with the first thing though!


Memoize is AMAZING


Make it flat: have an accumulator. A bunch of if statements that add to it, and/or branch
Check your variables once!

Don't use else, just a bunch of if statements

11211

1 1211 (4)
  1 1 211 (2)
    1 1 2 1 1 (1)
    1 1 2 11 (1)
  1 12 11 (1)
  1 12 1 1 (1)
11 211 (2)
  11 2 1 1 +
  11 2 11 +

11 211

  1,1 211
    1,1,2 11
      1,1,2,1,1 +
      1,1,2,11 +
    1,1,21,1 +
  1,12 11
    1,12,11 +
    1,12,1,1 +
11 211
  11,2 11
    11,2,1,1 +
    11,2,11 +
  11,21,1 +


1 1211
11 211

###

totalPossibleMessages = memoize (code) ->

  if not code then return 0

  char = code[0]
  double = char+code[1]

  total = 0

  if code.length is 1 and exists char
    total++

  if code.length is 2 and exists double
    total++

  if code.length > 1 and exists char
    rest = code.substr 1
    total += totalPossibleMessages rest

  if code.length > 2 and exists(double)
    rest = code.substr 2
    total += totalPossibleMessages rest

  return total

