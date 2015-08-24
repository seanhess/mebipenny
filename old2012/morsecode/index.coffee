max = _.max
memoize = _.memoize

reverseMap =
  A: "._"
  B: "_..."
  C: "_._."
  D: "_.."
  E: "."
  F: ".._."
  G: "__."
  H: "...."
  I: ".."
  J: ".___"
  K: "_._"
  L: "._.."
  M: "__"
  N: "_."
  O: "___"
  P: ".__."
  Q: "__._"
  R: "._."
  S: "..."
  T: "_"
  U: ".._"
  V: "..._"
  W: ".__"
  X: "_.._"
  Y: "_.__"
  Z: "__.."

map = {}
maxCodeLength = 0
for letter, code of reverseMap
  if code.length > maxCodeLength
    maxCodeLength = code.length
  map[code] = letter

readReverseLines (lines) ->
  code = readString lines
  possibles = allCodes code
  joined = possibles.map (p) -> p.join ''
  writeOutputs joined.sort()

allCodes = memoize (code) ->
  one = map[code.substr(0, 1)]
  two = map[code.substr(0, 2)]
  three = map[code.substr(0, 3)]
  four = map[code.substr(0, 4)]

  matches = []

  if code.length > 0 and one
    subcodes = allCodes code.substr(1)
    if not subcodes.length then matches.push one
    else
      for subcode in subcodes
        matches.push [one].concat subcode

  if code.length > 1 and two
    subcodes = allCodes code.substr(2)
    if not subcodes.length then matches.push two
    else
      for subcode in subcodes
        matches.push [two].concat subcode

  if code.length > 2 and three
    subcodes = allCodes code.substr(3)
    if not subcodes.length then matches.push three
    else
      for subcode in subcodes
        matches.push [three].concat subcode

  if code.length > 3 and four
    subcodes = allCodes code.substr(4)
    if not subcodes.length then matches.push four
    else
      for subcode in subcodes
        matches.push [four].concat subcode

  return matches



