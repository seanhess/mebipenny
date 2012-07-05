var http = require('./simplehttp')   
var querystring = require('querystring')

exports.paste = function(name, text, cb) {    
    
    var body = {
        api_option: "paste",
        api_user_key: "0fada1dcd905fa5775548075b947661e",  
        api_paste_private: "1",   
        api_paste_name: name, 
        api_paste_code: text,        
        api_paste_expire_date: "10M",
        api_paste_format: "txt"
    }

    http.debug = console.log
    http.post("http://pastebin.com/api/api_post.php", body, {}, function(err) {
           console.log("DONE")
    })
    
    
    
    // var api_dev_key          = '0fada1dcd905fa5775548075b947661e'; // your api_developer_key
    // $api_paste_code      = 'just some random text you :)'; // your paste text
    // var api_paste_private        = '1'; // 0=public 1=private
    // $api_paste_name          = 'justmyfilename.js'; // name or title of your paste
    // $api_paste_expire_date       = '10M';
    // $api_paste_format        = 'php';
    // $api_user_key            = ''; // if invalid key or no key is used, the paste will be create as a guest
    // $api_paste_name          = urlencode($api_paste_name);
    // $api_paste_code          = urlencode($api_paste_code);
    // 
    // 
    // $url                 = 'http://pastebin.com/api/api_post.php';
    // $ch              = curl_init($url);
    // 
    // curl_setopt($ch, CURLOPT_POST, true);
    // curl_setopt($ch, CURLOPT_POSTFIELDS, 'api_option=paste&api_user_key='.$api_user_key.'&api_paste_private='.$api_paste_private.'&api_paste_name='.$api_paste_name.'&api_paste_expire_date='.$api_paste_expire_date.'&api_paste_format='.$api_paste_format.'&api_dev_key='.$api_dev_key.'&api_paste_code='.$api_paste_code.'');
    // curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    // curl_setopt($ch, CURLOPT_VERBOSE, 1);
    // curl_setopt($ch, CURLOPT_NOBODY, 0);
    // 
    // $response            = curl_exec($ch);
    // echo $response;
}


exports.paste("name", "some stuff", function() {
    console.log("DONE")
})