var assert = require('assert')


readLines(function(line, isFirst) {  
    if (isFirst) return
    var result = canPermute(line)
    if (result) console.log("Y")
    else console.log("N")
})                        
    
function canPermute(line) {
    var info = parseLine(line) 
    var result = info.result
    var numbers = info.numbers         
    
    // you have to use all the numbers. So, going through all the permutations is simply going through all the options. 
    // but you DO have branches, based not on the array, but on the operations you've tried / to try
    
    // would be nice to exit early if you find one. 
    
    var combos = findCombos(numbers)
    
    for (var i = 0; i < combos.length; i++) {
        if (combos[i] === result) return true
    }

    return false
}    

function findCombos(startingNumbers) {   
    // if (!startingNumbers.length) return []
    var numbers = startingNumbers.concat()
    var combos = []                       
    
    print("Find Combos", startingNumbers)    
    
    if (numbers.length) {
        if (numbers.length > 1) {
            for (var i = 0; i < numbers.length; i++) {
                var num = numbers[i]
                var subCombos = findCombos(remove(numbers, i))      
                print("adding sub combos", combos, num, subCombos)            
                subCombos.forEach(function(total) {
                    combos.push(total * num)
                    combos.push(total - num)
                    combos.push(num - total)
                    combos.push(total + num)                                        
                })                                  
                print("added sub combos", combos, num, subCombos)            
            }                             
        }                     
        else {
            combos.push(numbers[0])
        }
    }
    
    print("Found Combos", startingNumbers, combos)                     
    
    return combos
}

function parseLine(line) {
    var numbers = line.split(/\s/)
    var result = parseInt(numbers.pop(), 10)
    
    numbers = numbers.map(function(num) {
        return parseInt(num, 10)
    })
    
    return {
        numbers: numbers,
        result: result
    }
}     

function print() {
    // console.log.apply(null, arguments)
}



if (module == require.main) {                     
    assert.equal(canPermute("10 2 8"), true)    
    assert.equal(canPermute("2 10 8"), true)        
    assert.equal(canPermute("1 2 3 4 0"), true)     
    assert.equal(canPermute("1 2 3 -5"), true)    
    assert.equal(canPermute("1 2 3 2"), true)        
    assert.equal(canPermute("3 2 1 -5"), true)    
    assert.equal(canPermute("3 2 1 2"), true)        
    assert.equal(canPermute("3 1 2 -5"), true)    
    assert.equal(canPermute("3 1 2 -4"), true)        
    assert.equal(canPermute("3 1 2 -7"), false)            
    assert.equal(canPermute("3 3 3 3"), true)     
    assert.equal(canPermute("1 2 3 125"), false)     
    assert.equal(canPermute("1 2 0"), false)         
    assert.equal(canPermute("8 2 8 8"), true)    
    assert.equal(canPermute("10 10 9 9 1 8"), true)
    assert.equal(canPermute("10 10 9 9 1 17"), true)     
}              
















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

