http = require 'http'
urlmod = require 'url'

post = (url, body, cb) ->
  cb ?= ->
  urlinfo = urlmod.parse url

  bodyString = JSON.stringify(body)

  options =
    host: urlinfo.hostname
    port: urlinfo.port
    path: urlinfo.path
    method: "POST"
    headers:
      "Content-Type": "application/json"
      "Content-Length": bodyString.length

  req = http.request options, (res) ->
    result = ""
    res.on 'data', (data) -> result += data
    res.on 'end', -> cb null, result
    res.on 'error', (err) -> 
      cb err

  req.write bodyString, 'utf8'
  req.end()

shout = (body, cb) -> post "http://radiant-temple-9093.herokuapp.com/", body, cb

#shout {message: "hello"}, ->

#post "http://radiant-temple-9093.herokuapp.com/", {key: "value"}, (err, asdf) ->
  #console.log err, asdf
# http://radiant-temple-9093.herokuapp.com/

