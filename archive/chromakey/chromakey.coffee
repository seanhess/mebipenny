
readReverseLines (lines) ->
  width = nextLine lines, toInt
  height = nextLine lines, toInt

  numPixels = width * height

  base = nextLines lines, numPixels, toRGB
  overlay = nextLines lines, numPixels, toRGB

  both = zip base, overlay

  chromad = both.map ([basePixel, overlayPixel]) ->
    pixel = if greaterGreen basePixel then overlayPixel
    else basePixel
    [pixel.r, pixel.g, pixel.b].join(" ")

  writeOutputs chromad

greaterGreen = ({r, g, b}) ->
  g > r + b

toRGB = (line) ->
  [r, g, b] = words(line).map(toInt)
  return {r, g, b}





