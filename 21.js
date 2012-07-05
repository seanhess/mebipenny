var assert = require('assert')


                          
readLines(function(line, isFirst) {  
    if (isFirst) return
    console.log(bestHand(line))
})                        

// returns the best hand possible for those cards
function bestHand(cardLine) {
    var cards = parseCards(cardLine)
    
    // naive: run every possible combination, including 1 and 11
    // better: run through the tree of options, recursively. 

    // picks the best SINGLE card right now
    function bestHandWithStartingScoreAndCards(currentScore, cards) {        
        if (currentScore > 21) return currentScore
        var best = currentScore
        for (var i = 0; i < cards.length; i++) {
            var card = cards[i]
            
            if (card == "A") {
                var combinedWithOne = bestHandWithStartingScoreAndCards(1 + currentScore, remove(cards, i)) 
                if (combinedWithOne > 21) continue
                if (combinedWithOne > best) best = combinedWithOne
                card = 11 // run it normally                
            }
                 
            var combined = bestHandWithStartingScoreAndCards(card + currentScore, remove(cards, i))
            if (combined > 21) continue
            if (combined > best) best = combined
        }                                       
        
        return best
    }   
    
    return bestHandWithStartingScoreAndCards(0, cards)
}   

function parseCards(cardLine) { 
    var cards = []
    
    for (var i = 0; i < cardLine.length; i++) {
        var letter = cardLine[i]

        if (letter.match(/\d/)) 
            cards.push(parseInt(letter, 10))
            
        else if (letter.match(/J|Q|K/))
            cards.push(10)
            
        else {            
            cards.push("A")
        }
            
    }        
    
    return cards
}


            



if (module == require.main) {
    assert.equal(bestHand("296J"), 21)
    assert.equal(bestHand("85"), 13)
    assert.equal(bestHand("A37"), 21)
    assert.equal(bestHand("475A"), 20)        
    assert.equal(bestHand("475JKA"), 21)     
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

