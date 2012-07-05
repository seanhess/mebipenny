var assert = require('assert')
              
parseAll()

function parseAll() {
    var baseImage = []
    var overlayImage = []
    var width = 0
    var height = 0
    var totalPixels = null
    readLines(function(line, lineIndex) {    
        var lineNums = parseLine(line)        

        if (lineIndex === 0) {
            width = lineNums.r
        }
        
        else if (lineIndex === 1) {     
            height = lineNums.r
            totalPixels = width * height
        }               
        
        else if (lineIndex > 1 && lineIndex < 2 + totalPixels)  {
            console.log(lineNums)
            baseImage.push(lineNums)
        }                           
        
        else {
            overlayImage.push(lineNums)
        }                            
        
        if (overlayImage.length == totalPixels) {
            console.log("READY", baseImage)
            var combined = chromakey(baseImage, overlayImage)
            combined.forEach(function(pixel) {
                console.log(pixel.r + " " + pixel.g + " "+ pixel.b)
            })
        }
    })    
}      

function parseLine(line) {
    
    var split = line.split(/\s+/)
    return {
        r: parseInt(split[0], 10),
        g: parseInt(split[1], 10),
        b: parseInt(split[2], 10)                
    }
}

function chromakey(base, overlay) {
    var combined = []
    for (var i = 0; i < base.length; i ++) {
        var basePixel = base[i]
        var overlayPixel = overlay[i]
        
        if (basePixel.g > basePixel.r + basePixel.b) {
            combined.push(overlayPixel)
        }                             
        else {
            combined.push(basePixel)
        }
    }                              
    
    return combined
}
              
















function debug() {
    console.log.apply(null, arguments)
}



// Lib stuff  

function readLines(eachLine) {
    var isFirstLine = true     
    var lineIndex = 0 
                         
    // backwards compatible to 0.2.6 stdin reading
    var stdin = process.openStdin();
    stdin.setEncoding('utf8');
                                      
    stdin.on('data', function (chunk) {
        var chunks = chunk.toString().split("\n")
        chunks.forEach(function(line) {
            if (line) {
                eachLine(line, lineIndex++)
                isFirstLine = false                         
            }
        })
    })                                                                                 
}


// returns a copy of the array, minus the index specified
function remove(array, index, length) {   
    if (typeof length == "undefined") length = 1
    var copy = array.concat()
    copy.splice(index, length)
    return copy
}                        

