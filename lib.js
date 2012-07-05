// Lib stuff  

function readLines(eachLine) {
    var isFirstLine = true      
                         
    // backwards compatible to 0.2.6 stdin reading
    var stdin = process.openStdin();
    stdin.setEncoding('utf8');
                                      
    stdin.on('data', function (chunk) {
        var chunks = chunk.toString().split("\n")
        chunks.forEach(function(line) {
            if (line) {
                eachLine(line, isFirstLine)
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

