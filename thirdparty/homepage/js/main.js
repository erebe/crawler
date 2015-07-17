'use strict';

function loadReddit(container, json)
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

function loadWeather(container, json)
{
    var weathers = json.filter(function(city) { return city.forecasts.length; } )
                     .map(function(city) {
                         return city.forecasts.map(function(forecast) {
                             return generateWeatherView(city.city, forecast); });
                     });

    container.append.apply(container, weathers);
}

function loadAnimes(container, json)
{
    var animes = json.filter(function(anime) { return anime.episodes.length; } )
                     .sort(function(a,b) { return b.episodes[0].date - a.episodes[0].date;} )
                     .map(function(anime) {
                         return anime.episodes.map(function(episode) {
                             return generateAnimeView(anime, episode); });
                     });


    container.append.apply(container, animes);
}

function loadVideos(container, json)
{
    var videos = json.filter(function(channel) { return channel.videos.length; } )
                     .map(function(channel) {
                         return channel.videos.map(function(video) {
                             return generateVideoView(channel.name, video); });
                     });


    container.append.apply(container, videos);

}

function loadSeries(container, json)
{
    var series = json.filter(function(serie) { return serie.episodes.length} )
                     .sort(function(a,b) { return b.episodes[0].date - a.episodes[0].date;} )
                     .map(function(serie) {
                        return serie.episodes.map(function(episode) {
                            return generateSerieView (serie.name, episode);

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
            : "⇒";
    }


    var row = $('<tr></tr>');
    var cell = $('<th></th>');
    var row_bullet = $('<th class="bullet">'+ generateThumbnail(topic)+ '</th>');
    var link = $('<a href="' + topic.url + '"><b>'+ topic.title + '</b></a>' +
        '<br/><a style="font-size:12px;" href="'+ topic.commentLink + '"> ' + topic.numComments + ' comments </a>');

    cell.append(link);
    return row.append.apply(row, [row_bullet, cell]);
}

function generateWeatherView(cityName, forecast) {

    var container = $('<div class="item weather-item"></div>');
    var header = $('<header><h1>' + cityName + '</h1></header>')

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
        $.getJSON("/api/forecast/" + encodeURIComponent(cityName), 
                loadWeather.bind(undefined, openPanel(cityName)));
    });

    return container.append(header, thumbnail, footer);
}


function generateAnimeView(anime, episode)
{
    var container = $("<div class='item anime-item'></div>");

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
                         '<h3>' + episode.title + '</h3>' +
                         '<h3>' + date.toLocaleDateString() + '</h3>' +
                     '</a>' +
                   '</footer>');

    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/anime/" + encodeURIComponent(anime.name), 
                loadAnimes.bind(undefined, openPanel(anime.name)));
    });


    return container.append(header, thumbnail, footer);
}

function generateSerieView(serieName, episode)
{
    var container = $("<div class='item serie-item'></div>");
    var header = $('<header>' +
                   '<a href=""><h1>' + serieName + '</h1></a>' +
                   '</header>');

    var date = new Date(episode.date * 1000);
    var footer = $('<footer class="footer">' +
                        '<a href="' + episode.magnetURI + '">' +
                            '<h3>' + episode.title + '</h3>' +
                            '<h3>' + date.toLocaleDateString() + '</h3>' +
                        '</a>' +
                    '</footer>'
                   );


    header.click(function(event) {
        event.preventDefault();
        $.getJSON("/api/serie/" + encodeURIComponent(serieName),
                loadSeries.bind(undefined, openPanel(serieName)));
    });


    return container.append(header, footer);

}

function generateVideoView(channelName, video)
{
    var container = $('<div class="item videoItem"></div>');
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
        $.getJSON("/api/youtube/" + encodeURIComponent(channelName), 
                loadVideos.bind(undefined, openPanel(channelName)));
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
    $.getJSON("/api/youtube/",  loadVideos.bind(loadVideos, $("#videos")));
    $.getJSON("/api/serie/",    loadSeries.bind(loadSeries, $("#series")));
    $.getJSON("/api/anime/",    loadAnimes.bind(loadAnimes, $("#animes")));
    $.getJSON("/api/forecast/", loadWeather.bind(loadWeather, $("#weathers")));
    $.getJSON("/api/reddit/",   loadReddit.bind(loadReddit, $("#reddits")));


    var contentSections = $('.cd-section'),
        navigationItems = $('#cd-vertical-nav a');

    updateNavigation();
    $(window).on('scroll', function(){
        updateNavigation();
    });

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
    //smooth scroll to the section
    navigationItems.on('click', function(event){
        event.preventDefault();
        smoothScroll($(this.hash));
    });
    //smooth scroll to second section
    $('.cd-scroll-down').on('click', function(event){
        event.preventDefault();
        smoothScroll($(this.hash));
    });

    //open-close navigation on touch devices
    $('.touch .cd-nav-trigger').on('click', function(){
        $('.touch #cd-vertical-nav').toggleClass('open');

    });
    //close navigation on touch devices when selectin an elemnt from the list
    $('.touch #cd-vertical-nav a').on('click', function(){
        $('.touch #cd-vertical-nav').removeClass('open');
    });

    function updateNavigation() {
        contentSections.each(function(){
            var th = $(this);
            var activeSection = $('#cd-vertical-nav a[href="#'+th.attr('id')+'"]').data('number') - 1;
            if ( ( th.offset().top - $(window).height()/2 < $(window).scrollTop() ) && ( th.offset().top + th.height() - $(window).height()/2 > $(window).scrollTop() ) ) {
                navigationItems.eq(activeSection).addClass('is-selected');
            }else {
                navigationItems.eq(activeSection).removeClass('is-selected');
            }
        });
    }

    function smoothScroll(target) {
        $('body,html').animate(
            {'scrollTop':target.offset().top},
            600
        );
    }

});
