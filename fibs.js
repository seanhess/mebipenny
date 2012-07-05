var assert = require('assert')
             
// needs to generate fib numbers up to B, 
// then return all that are between. 
var fibs = [0, 1]                
parseAll()

function parseAll() {
    generateFibsUpTo(2000000000000000000000000000000000000000000000000) // precalculate them
    readLines(function(line, lineIndex) {    
        if (lineIndex > 0) {        
            var info = parseLine(line)    
            console.log(fibsBetween(info.a, info.b))
        }
    })    
}    

function fibsBetween(a, b) {
    
    var found = 0;
    
    for (var i = 0; i < fibs.length; i++) {
        var fib = fibs[i]                       
        if (a <= fib && fib <= b) 
            found++
            
        if (fib > b)
            return found
    }           
    
    return found
}   
        
function parseLine(line) {
    var split = line.split(/\s+/)
    return {
        a: parseInt(split[0], 10),
        b: parseInt(split[1], 10)
    }
}

function generateFibsUpTo(num) {

	var var1 = 0;
	var var2 = 1;
	var var3;
                 
	for(var i=3; lastFib() <= num;i++)
	{
		var3 = var1 + var2;
		var1 = var2;
		var2 = var3;

        fibs.push(var3)
	}    
} 

function lastFib() {   
    return fibs[fibs.length-1]
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

