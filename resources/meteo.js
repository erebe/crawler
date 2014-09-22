// ==UserScript==
// @name meteo
// @namespace api
// @include https://api.erebe.eu/meteo/*
// @version 1
// @grant none
// ==/UserScript==

function loadCssFile(filename) {
var fileref=document.createElement("link")
  fileref.setAttribute("rel", "stylesheet")
  fileref.setAttribute("type", "text/css")
  fileref.setAttribute("href", filename)
  document.getElementsByTagName("head")[0].appendChild(fileref)
}

document.body.onload = function () {
  var jsonText = document.body.childNodes[0].innerHTML;
  var json = JSON.parse(jsonText);
  document.body.innerHTML = '';
  var str = '<div>';
  for (var i in json) {
    if (!json[i].forecasts.length) continue;
    str += '<div style="float: left;margin-left: 50px;">';
    str += '<h2>' + json[i].city + '</h2>';
    for (var j in json[i].forecasts) {
      str += '<span style="display:inline-block;margin-right:50px;">'
       str += '<p>' + json[i].forecasts[j].date + '</p>';
      str += '<div style="padding-left:25%"><img src="' + json[i].forecasts[j].iconUrl + '"/></div>';
      str += '<p style="text-align:center;">'  
                   +  json[i].forecasts[j].description
                   + ' (' + json[i].forecasts[j].temperature + 'C°)</p>';
       str += '</span>';
    }
    str += '</div>';
  }
  console.log(str);
  str += '</div>';
  loadCssFile("https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css");
 document.body.innerHTML += str;
  
};
