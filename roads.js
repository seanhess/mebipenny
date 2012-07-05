var assert = require('assert')

parseAll()               

// assert.equal(2, findRouteDistance({
//     coins: 5,
//     dest: 2,
//     roads: [{
//         source: 1,
//         dest: 2,
//         length: 2,
//         toll: 3
//     }]
// }))      
// 
// assert.equal(-1, findRouteDistance({
//     coins: 1,
//     dest: 2,
//     roads: [{
//         source: 1,
//         dest: 2,
//         length: 2,
//         toll: 3
//     }]
// }))      

// assert.equal(4, findRouteDistance({
//     coins: 5,
//     dest: 3,
//     roads: [
//         {source: 1, dest: 2, length: 2, toll: 3},
//         {source: 2, dest: 3, length: 2, toll: 1}    
//     ]
// }))      

// assert.equal(2, findRouteDistance({
//     coins: 5,
//     dest: 3,
//     roads: [
//         {source: 1, dest: 2, length: 2, toll: 3},
//         {source: 2, dest: 3, length: 2, toll: 1},    
//         {source: 1, dest: 3, length: 2, toll: 1}        
//     ]
// }))   

// assert.equal(4, findRouteDistance({
//     coins: 5,
//     dest: 3,
//     roads: [
//         {source: 1, dest: 2, length: 2, toll: 3},
//         {source: 2, dest: 3, length: 2, toll: 1},    
//         {source: 1, dest: 3, length: 2, toll: 6}        
//     ]
// }))       

function parseAll() {
    var numCountries = -1          
    var currentCountryStart = 1    
    var currentCountryLast = -1
    var currentCountry = {roads: []} 
    var countries = []
    readLines(function(line, lineIndex) {    
        if (lineIndex === 0) {
            numCountries = parseNum(line)
        }        

        else if (lineIndex === currentCountryStart){
            currentCountry.coins = parseNum(line)
        }
        
        else if (lineIndex === currentCountryStart+1) {
            currentCountry.dest = parseNum(line)
        }                                            
        
        else if (lineIndex === currentCountryStart+2) { 
            currentCountry.numRoads = parseNum(line)
            currentCountryLast = currentCountryStart + 2 + currentCountry.numRoads
        }                                           
        
        else {
            currentCountry.roads.push(parseRoad(line))
        }          
        
        if (lineIndex == currentCountryLast) {
            // debug("COUNTRY", lineIndex, currentCountry)
            currentCountryStart = lineIndex + 1
            countries.push(currentCountry)            
            currentCountry = {roads: []}
        }
        
        
        if (countries.length == numCountries) {
            countries.forEach(function(country) {
                console.log(findRouteDistance(country))
            })
        }
    })    
}   
        

function mapRoutes(country) {
    var routes = []
    country.roads.forEach(function(road) {
        routes[road.source] = routes[road.source] || []
        routes[road.source].push(road)
    })      
                              
    country.routes = routes
    country.bestDistances = []
}



function findRouteDistance(country) {   
    mapRoutes(country)        
    return findBestDistance(country, 1, country.dest, country.coins)
}      

// return the BEST distance lower than the cost
// of all cities that make it there.   

function findBestDistance(country, startingCity, endingCity, coinsLeft) {   
    
    function findBestInner(startingCity, coinsLeft, visited) {     

        if (visited[startingCity]) return -1 
        
        visited[startingCity] = true

        if (startingCity == endingCity) 
            return 0

        var routes = country.routes

        debug("FIND BEST DISTANCE start=" +startingCity + " coins=" +coinsLeft)        

        var bestDistance = -1;   

        if (!routes[startingCity]) return -1

        for (var i = 0; i < routes[startingCity].length; i++) {          

            var road = routes[startingCity][i]

            if (!road) continue;

            // console.log(" - road ", startingCity, road)        

            if (road.toll > coinsLeft) {  
                debug(" - out of coins ", road)
                continue; // can't go this way!
            }          

            debug("Following Route: ", road)            
            var subRouteDistance = findBestDistance(country, road.dest, endingCity, coinsLeft - road.toll, visited.concat())

            if (subRouteDistance < 0) {
                debug(" - dead end ", road)
                continue;
            }


            var routeDistance = road.length + subRouteDistance 
            debug("Route Distance: ", road, routeDistance)                                    

            if (routeDistance < bestDistance || bestDistance === -1) {
                bestDistance = routeDistance
            }

        }  

        country.bestDistances[startingCity] = bestDistance
        debug("going back", bestDistance)
        
        return bestDistance        
    }
    
                        
    
    return findBestInner(startingCity, coinsLeft, [])
}  
       

function parseRoad(line) {
    var split = line.split(/\s+/)
    return {
        source: parseInt(split[0], 10),
        dest: parseInt(split[1], 10),
        length: parseInt(split[2], 10),
        toll: parseInt(split[3], 10)        
    }
} 
         
function dummyRoad() {
    return {
        source: 1,
        dest: 2,
        length: 2,
        toll: 3
    }    
}    

function dummyRoutes() {
    return [dummyRoad()]
}













                             

function parseNum(line) {
    return parseInt(line, 10)
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

