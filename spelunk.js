var assert = require('assert')

// It looks like it's working to me. Maybe I'm not understanding the problem correctly. Obviously :)
            
parseAll()    

// assert.equal(19, maxCapacity(4, [
//     {start: 1, end: 2, capacity: 4},
//     {start: 1, end: 3, capacity: 5},    
//     {start: 1, end: 4, capacity: 7},        
//     {start: 1, end: 2, capacity: 10},            
//     {start: 2, end: 4, capacity: 12}
// ]))

function parseAll() {
    var levels = -1    
    var numCorridors = -1
    var corridors = []
    readLines(function(line, lineIndex) {    
        if (lineIndex === 0) {
            levels = parseInt(line, 10)
        }         
        else if (lineIndex === 1) {
            numCorridors = parseInt(line, 10)
        }
        else if (lineIndex > 1) {
            corridors.push(parseLine(line))
        }
        
        if (corridors.length == numCorridors) {
            debug("DONE", levels, numCorridors, corridors)
            var max = maxCapacity(levels, corridors)
            console.log(max)
        }
    })    
}   

function maxCapacity(numLevels, corridors) {
    var levels = []                
    var nodes = []   
    
    var caps = []
    
    corridors.forEach(function(c) {
        nodes[c.start] = nodes[c.start] ||[]
        nodes[c.start].push(c)
    })                             
    
    // console.log(nodes)
    
    function maxAtLevel(level) {     
        // console.log("MAx AT LEVEL", level)
        
        if (caps[level]) {
            console.log("WAHOO", level, caps)
            return caps[level]
        }
        var exits = nodes[level]
        
        if (!exits) return 99999999

        var maximumCapacity = 0
        
        exits.forEach(function(c) {
            var subCapacity = maxAtLevel(c.end)
            
            // console.log("SUB CAPACITY", c.end, subCapacity)            
            
            var capacity = (subCapacity < c.capacity) ? subCapacity : c.capacity
            
            // console.log("LEvel CAPACITY", level, c.end, subCapacity)                        
            
            maximumCapacity += capacity
        })   
                   
        caps[level] = maximumCapacity
        return caps[level]
    }
    
    return maxAtLevel(1)
}                                          

function parseLine(line) {
    var split = line.split(/\s+/)
    return {
        start: parseInt(split[0], 10),
        end: parseInt(split[1], 10),
        capacity: parseInt(split[2], 10)
    }
}










function debug() {
    // console.log.apply(null, arguments)
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

