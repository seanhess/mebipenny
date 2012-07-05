// Simple Http Client
// - automatic Host header
// - https


var http = require('http')
var sys = require('sys')
var url = require('url')
var querystring = require('querystring')
var _ = require('underscore')

exports.OK = 200
exports.NoContent = 204
exports.Found = 302
exports.MovedPermanently = 301
exports.BadRequest = 400

exports.StatusCodeMessages = {}
exports.StatusCodeMessages[exports.OK] = "OK"
exports.StatusCodeMessages[exports.Found] = "Found"
exports.StatusCodeMessages[exports.MovedPermanently] = "Moved Permanently"
exports.StatusCodeMessages[exports.BadRequest] = "Bad Request"
exports.StatusCodeMessages[exports.NoContent] = "No Content"

// override with sys.puts, or sys.debug or a custom function to see traffic
exports.debug = function(message) {}

function getStatusCodeMessage(code) {
    return exports.StatusCodeMessages[code] || ""
}

exports.get = function(urlString, headers, cb) {
    send("GET", urlString, headers, cb)
}

// Automatically serializes JSON objects and sets the 
// appropriate headers if necessary
// headers is optional
exports.post = function(urlString, headers, body, cb) {
    
    if(_(body).isFunction()) {
        cb = body
        body = headers
        headers = {}
    }
    
    exports.send("POST", urlString, headers, body, cb)    
}

exports.put = function(urlString, headers, body, cb) {
    
    if(_(body).isFunction()) {
        cb = body
        body = headers
        headers = {}
    }
    
    exports.send("PUT", urlString, headers, body, cb)    
}

exports.del = function(urlString, headers, cb) {
    send("DELETE", urlString, headers, cb)
}

// headers is optional
// body is optional
var send = exports.send = function(method, urlString, headers, body, cb) {
    var parsedUrl = new Url(urlString)
    var client = new Client(parsedUrl)
    
    // sys.puts("SEND " + method + " " + parsedUrl.pathAndQuery + " " + headers + " " + body + " " + cb)
    
    client.send(method, parsedUrl.pathAndQuery, headers, body, cb)
}

exports.getStatusCode = function(urlString, cb) {
    var parsedUrl = new Url(urlString)
    var client = new Client(parsedUrl)
    
    client.sendRich({path: parsedUrl.pathAndQuery, statusCode: true}, cb)
}

Client = function(url) {
    
    if (!(url instanceof Url)) {
        url = new Url(url)
    }
    
    var client = http.createClient(url.port, url.hostname, url.https)
    
    // headers
    var automaticHeaders = { "Host" : url.hostname }
    
    this.send = function(method, path, headers, body, cb) {
		if (_(headers).isFunction()) {
            cb = headers
            body = ""
            headers = {}
        }
        
        else if (_(body).isFunction()) {
            cb = body
            body = ""
        }
		this.sendRich({method: method, path: path, headers: headers, body: body}, cb)
	}
	
	//  Params:
	// method - optional, default GET
	// path - required
	// headers - optional
	// body - optional
	// statusCode - (just call back with status code) - optional - default false
	this.sendRich = function(params, cb) {
		params.body || (params.body = "")
		params.headers || (params.headers = {})
		params.method || (params.method = "GET")
        
        // Stringify Body 
        if (!_(params.body).isString()) {
            params.body = JSON.stringify(params.body)
            params.headers["Content-Type"] = "application/json"
            params.headers["Accept"] = "application/json" // wild assumptions here :)
        }
        
        params.headers["Content-Length"] = params.body.length
        
        params.headers = _(automaticHeaders).extend(params.headers)
        
        var request = client.request(params.method, params.path, params.headers)
        
        request.socket.on('error', function(err) {
            cb(err)
        })
        
        exports.debug(">>>>>>>>>>>>>\n" + url.port + " " + url.hostname + " " + url.https + "\n" + request._header)
        exports.debug(params.body)
        
        request.on('error', function(err) {
            cb(err)
        })
        
        request.on('close', function(hadError) {
            // sys.debug("CLOSE " + hadError)
        })
        
        request.on("timeout", function() {
            // sys.debug("Timeout")
        })
        
        request.on('end', function() {
            // sys.debug("END")
        })

        request.on('response', function(response) {
            
            // sys.puts("RESPONSE")
            
            var data = ""

            response.on('data', function(chunk) {
                data += chunk
            })

            response.on('end', function() {
                var responseObject = new Response(response, data)
                // Automatically Redirect
                if (params.method == "GET" && response.statusCode == exports.Found || response.statusCode == exports.MovedPermanently) {
					var client = new Client(new Url(response.headers.location))					
					return client.sendRich({path: response.headers.location, statusCode: params.statusCode}, cb)
                }                
                
                exports.debug("<<<<<<<<<<<<\n" + responseObject)
                cb(null, params.statusCode ? response.statusCode : responseObject)
            })
        })
        
        var asdf = request.end(params.body, "utf8")        
    }
    
    this.get = function(path, headers, cb) {
        this.send('GET', path, headers, cb)
    }
}

var Response = function(response, data) {
	
	this.parsedJson = null
    
    this.__defineGetter__('data', function() {          return data                     })
    this.__defineGetter__('statusCode', function() {    return response.statusCode      })    
    this.__defineGetter__('headers', function() {       return response.headers         })
    
    this.__defineGetter__('json', function() {
		if (!this.parsedJson) {
	        try {
				this.parsedJson = JSON.parse(data)
	        }
	        catch (e) { 
				// ignore, just return nil
				console.log("SIMPLEHTTP JSON ERROR ",e)
			}			
		}
		return this.parsedJson
    })
    
    this.toString = function() {
        var out = "HTTP/" + response.httpVersion + " "+ response.statusCode + " " + getStatusCodeMessage(response.statusCode)
        
        for (var header in response.headers)
            out += "\n" + header + ": " + response.headers[header]
            
        out += "\n\n" + data
                
        return out
    }
}

var Url = function(urlstring) {
    
    var parsedUrl = url.parse(urlstring)
            
    this.__defineGetter__('baseUrl', function() {   return parsedUrl.protocol + "//" + host     })
    this.__defineGetter__('hostname', function() {  return parsedUrl.hostname                   })
    this.__defineGetter__('path', function() {      return parsedUrl.pathname || "/"             })
    this.__defineGetter__('query', function() {     return parsedUrl.query  || ""               })
    this.__defineGetter__('search', function() {    return parsedUrl.search  || ""              })
    this.__defineGetter__('https', function() {     return (parsedUrl.protocol == "https:")     })
    this.__defineGetter__('href', function() {      return parsedUrl.href                       })
    
    this.__defineGetter__('port', function() {
        if (parsedUrl.port) return parsedUrl.port
        if (this.https) return 443
        else return 80
    })

    this.__defineGetter__('pathAndQuery', function() { 
        return this.path + this.search
    })
    
    this.toString = function() {                    return urlString                            }
}

exports.Response = Response
exports.Url = Url

if (module == require.main) {
    exports.getStatusCode("http://www.tvsquad.com/media/2006/02/bonnieraitt_thumbnail.jpg", function(err, response) {
        sys.puts("code " + response)
    })
}
