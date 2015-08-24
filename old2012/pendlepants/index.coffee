without = _.without
flatten = _.flatten

readReverseLines (lines) ->
  wardrobe = []
  while ((string = readString lines))
    wardrobe.push string.split(" ")
  outfits = uniqueOutfits wardrobe
  lines = outfits.map (outs) -> outs.join " "
  writeOutputs lines

# you don't know how many body parts there are
uniqueOutfits = (wardrobe) ->
  #return allPossibleCases wardrobe
  if wardrobe.length is 0 then return []


  #for type in wardrobe
  type = wardrobe[0]

  uniques = []

  otherTypes = without wardrobe, type
  subs = uniqueOutfits otherTypes
  for cloth in type
    if not subs.length
      uniques.push [cloth]
    else
      for sub in subs
        uniques.push [cloth].concat(sub)

  return uniques


