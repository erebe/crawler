// ==UserScript==
// @name        yotube
// @namespace   api
// @include     https://api.erebe.eu/youtube/last
// @include     https://api.erebe.eu/youtube/
// @version     1
// @grant       none
// ==/UserScript==
document.body.onload = function () {

    var jsonText = document.body.childNodes[0].innerHTML;
    var json = JSON.parse(jsonText);
    
    //document.body.removeChild(document.body.childNodes[0]);
    document.body.innerHTML = '';
    var str = '<div>';
    for (var i in json) {
        if(!json[i]._videos.length) continue;
        str += '<div style="float: left;margin-left: 50px;">';
        str += '<h2>' + json[i]._name + '</h2>';
        str += '<a href="' + json[i]._videos[0]._url + '">';
        str += '<img src="' + json[i]._videos[0]._thumbnail + '" />';
        str += '<p>' + json[i]._videos[0]._titre + '</p>';
        str += '</a>';
        str += '</div>';
    }
     console.log(str);

    str += '</div>';
    document.body.innerHTML += str;
};
