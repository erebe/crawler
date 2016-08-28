'use strict';

function prepareReddit(container, json)
{
    var subs = json.filter(function(sub) { return sub.topics.length; } )
                   .map(function(sub) {
                       var table = $("<table class='item reddit-item'><thead><tr><th>"
                                      + "<h1><a href='https://reddit.com/r/" + sub.name + "'>"+ sub.name +"</a></h1>"
                                      + "</th></tr></thead></table>"
                                    );
                        return table.append.apply(table, sub.topics.map(function(topic) {
                            return generateRedditView(sub.name, topic); }));
                     });

    container.append.apply(container, subs);

}

function prepareWeather(container, json)
{
    var weathers = json.filter(function(city) { return city.forecasts.length; } )
                     .map(function(city) {
                         return city.forecasts.map(function(forecast) {
                             return generateWeatherView(city.city, forecast); });
                     });

    container.append.apply(container, weathers);
}

function prepareAnimes(container, json)
{
    var animes = json.filter(function(anime) { return anime.episodes.length; } )
                     .sort(function(a,b) { return b.episodes[0].date - a.episodes[0].date;} )
                     .map(function(anime) {
                         return anime.episodes.map(function(episode) {
                             return generateAnimeView(anime, episode); });
                     });


    container.append.apply(container, animes);
}

function prepareVideos(container, json)
{
    var videos = json.filter(function(channel) { return channel.videos.length; } )
                     .map(function(channel) {
                         return channel.videos.map(function(video) {
                             return generateVideoView(channel.name, video); });
                     });


    container.append.apply(container, videos);

}

function prepareSeries(container, json)
{
    var series = json.filter(function(serie) { return serie.episodes.length} )
                     .sort(function(a,b) { return b.episodes[0].date - a.episodes[0].date;} )
                     .map(function(serie) {
                        return serie.episodes.map(function(episode) {
                            return generateSerieView (serie, episode);

                        });
                    });


    container.append.apply(container, series);

}


function generateRedditView(subName, topic)
{
    var generateThumbnail = function(topic) {
        var url = topic.thumbnail;
        return (url.length && url != "self") ?
            '<a href="'+ topic.url + '"><img style="vertical-align: middle;" src="' + url  + '"/></a>'
            : "";
    };


    var row = $('<tr>');
    var cell = $('<td>');
    var row_bullet = $('<td>', {"class": "bullet", html: generateThumbnail(topic)});
    var link = $('<a href="' + topic.url + '"><b>'+ topic.title + '</b></a>' +
        '<br/><a style="font-size:12px;" href="'+ topic.commentLink + '"> ' + topic.numComments + ' comments </a>');

    cell.append(link);
    return row.append.apply(row, [row_bullet, cell]);
}

function generateWeatherView(cityName, forecast) {

    var container = $('<div>', {'class': 'item weather-item'});
    var header = $('<header><h1>' + cityName + '</h1></header>');

    var thumbnail = $('<div class="thumbnail">' +
                        '<img src="' + forecast.iconUrl + '"/>' +
                      '</div>'
                     );

     var footer = $('<footer>' +
                        '<h3>' + forecast.temperature + 'C°  ' + forecast.description + '</h3>' +
                        '<a>' + forecast.date + '</a>' +
                    '</footer>');


    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/forecast/get/" + encodeURIComponent(cityName),
                prepareWeather.bind(undefined, openPanel(cityName)));
    });

    return container.append(header, thumbnail, footer);
}


function generateAnimeView(anime, episode)
{
    var container = $("<div>", {'class': 'item anime-item'});

    var header = $('<header>' +
                       '<a href=""><h1>' + anime.name + '</h1></a>' +
                   '</header>'
                  );

    var thumbnail = $('<div class="thumbnail">' +
                        '<a href="' + episode.magnetURI + '">' +
                        '<img src="' + anime.thumbnail + '" /></a>' +
                      '</div>'
                     );

    var date = new Date(episode.date * 1000);
    var footer = $('<footer>' +
                     '<a href="' + episode.magnetURI + '">' +
                         '<a>' + episode.title + '</a><br/>' +
                         '<a>' + date.toLocaleDateString() + '</a>' +
                     '</a>' +
                   '</footer>');

    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/anime/get/" + encodeURIComponent(anime.name),
                prepareAnimes.bind(undefined, openPanel(anime.name)));
    });


    return container.append(header, thumbnail, footer);
}

function generateSerieView(serie, episode)
{
    var container = $("<div>", {'class': 'item serie-item'});
    var header = $('<header>' +
                   '<a href=""><h1>' + serie.name + '</h1></a>' +
                   '</header>');

    var thumbnail = $('<div class="thumbnail">' +
                        '<a href="' + episode.magnetURI + '">' +
                        '<img src="' + serie.thumbnail + '" /></a>' +
                      '</div>'
                     );

    var date = new Date(episode.date * 1000);
    var footer = $('<footer class="footer">' +
                        '<a href="' + episode.magnetURI + '">' +
                            '<a>' + episode.title + '</a><br/>' +
                            '<a>' + date.toLocaleDateString() + '</a>' +
                        '</a>' +
                    '</footer>'
                   );


    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/serie/get/" + encodeURIComponent(serie.name),
                prepareSeries.bind(undefined, openPanel(serie.name)));
    });


    return container.append(header, thumbnail, footer);

}

function generateVideoView(channelName, video)
{
    var container = $('<div>', {'class': 'item videoItem'});
    var header = $('<div class="header">' +
                    '<a href="">' +
                        '<h1>' + channelName + '</h1>' +
                    '</a>' +
                    '</div>'
                  );

    var thumbnail = $('<div class="thumbnail">' +
                       '<a href="' + video.url + '">' +
                           '<img src="' + video.thumbnail + '"/>' +
                       '</a>' +
                     '</div>'
                    );

    var footer = $('<div class="footer">' +
                     '<a href="' + video.url + '">' +  video.title + '</a>' +
                   '</div>'
                 );


    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/youtube/get/" + encodeURIComponent(channelName),
                prepareVideos.bind(undefined, openPanel(channelName)));
    });


    return container.append(header, thumbnail, footer);
}

function openPanel(title) {
    $(".cd-panel-header-title").html(title);
    var panel = $(".cd-panel-content");
    panel.empty();
    panel.animate({ scrollTop: 0 }, 0);
    $('.cd-panel').addClass('is-visible');

    return panel;

}

jQuery(document).ready(function($) {
    $.getJSON("/api/youtube/last",  prepareVideos.bind(null, $("#youtube .inner")));
    $.getJSON("/api/serie/last",    prepareSeries.bind(null, $("#series .inner")));
    $.getJSON("/api/anime/last",    prepareAnimes.bind(null, $("#anime .inner")));
    $.getJSON("/api/reddit/last",   prepareReddit.bind(null, $("#reddit .inner")));



    //open the lateral panel
    $('.cd-btn').on('click', function(event){
        event.preventDefault();
        $('.cd-panel').addClass('is-visible');
    });
    //clode the lateral panel
    $('.cd-panel').on('click', function(event){
        if( $(event.target).is('.cd-panel') || $(event.target).is('.cd-panel-close') ) {
            $('.cd-panel').removeClass('is-visible');
            event.preventDefault();
        }
    });

    //open-close navigation on touch devices
    $('.touch .cd-nav-trigger').on('click', function(){
        $('.touch #cd-vertical-nav').toggleClass('open');

    });
    //close navigation on touch devices when selectin an elemnt from the list
    $('.touch #cd-vertical-nav a').on('click', function(){
        $('.touch #cd-vertical-nav').removeClass('open');
    });

    function smoothScroll(target) {
        $('body,html').animate(
            {'scrollTop':target.offset().top},
            600
        );
    }

});