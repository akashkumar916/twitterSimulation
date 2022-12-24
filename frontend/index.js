//user id : 123
function get_tweet(){
    console.log("testing");
    //document.write("testing");
    //API
    var request = new XMLHttpRequest()

    // Open a new connection, using the GET request on the URL endpoint
    request.open('GET', 'http://localhost:8083/get_tweets/user?userId=123', true)

    request.onload = function () {
      // Begin accessing JSON data here
      //accessing the Id
       var data = JSON.parse(this.response)
       var elem = document.getElementById('retweet'),
       elem.innerHTML(data.retweet)
    }

    // Send request
    request.send()

}
function get_subscriber(){
    console.log("testing");
    //document.write("testing");
    //API
    var request = new XMLHttpRequest()

    // Open a new connection, using the GET request on the URL endpoint
    request.open('GET', 'http://localhost:8083/get_subscriberuser?userId=123', true)

    request.onload = function () {
      // Begin accessing JSON data here
      //accessing the Id
       var data = JSON.parse(this.response)
            var elem = document.getElementById('subscriber'),
            elem.innerHTML(data.retweet)
    }

    // Send request
    request.send()

}
function get_retweet(){
    console.log("testing");
    //document.write("testing");
    //API
    var request = new XMLHttpRequest()

    // Open a new connection, using the GET request on the URL endpoint
    request.open('GET', 'http://localhost:8083/get_retweet?userId=123', true)

    request.onload = function () {
      // Begin accessing JSON data here
      //accessing the Id
      var data = JSON.parse(this.response)
      var elem = document.getElementById('retweet'),
      elem.innerHTML(data.retweet)
    }
    // Send request
    request.send()

}