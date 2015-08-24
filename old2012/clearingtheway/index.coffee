readReverseLines (lines) ->
  console.log "HI"
  strings = []
  while ((string = readString lines))
    strings.push string
  console.log strings
