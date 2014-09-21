// ==UserScript==
// @name        meteo
// @namespace   api
// @include     https://api.erebe.eu/meteo/
// @version     1
// @grant       none
// ==/UserScript==

document.body.onload = function () {
  var jsonText = document.body.childNodes[0].innerHTML;
  var json = JSON.parse(jsonText);

  document.body.innerHTML = '';
  var str = '<div>';
  for (var i in json) {
    if (!json[i].forecasts.length) continue;
    str += '<div style="float: left;margin-left: 50px;">';
    str += '<h2>' + json[i].city + '</h2>';
    str += '<img src="' + json[i].forecasts[0].iconUrl + '" />';
    str += '<p>' + json[i].forecasts[0].description + " (" + json[i].forecasts[0].temperature + 'C°)</p>';
    str += '</div>';
  }
  console.log(str);
  str += '</div>';
  document.body.innerHTML += str;
};
