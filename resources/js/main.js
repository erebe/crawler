jQuery(document).ready(function($) {
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
            $this = $(this);
            var activeSection = $('#cd-vertical-nav a[href="#'+$this.attr('id')+'"]').data('number') - 1;
            if ( ( $this.offset().top - $(window).height()/2 < $(window).scrollTop() ) && ( $this.offset().top + $this.height() - $(window).height()/2 > $(window).scrollTop() ) ) {
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


document.body.onload = function () {

    callAjax("/video/", function(data) { loadVideos(data, $("#videos")); });
    callAjax("/serie/", function(data) { loadSeries(data, $("#series")); });
    callAjax("/anime/", function(data) { loadAnimes(data, $("#animes")); });
    callAjax("/meteo/", function(data) { loadWeather(data, $("#weathers")); });
    callAjax("/reddit/", function(data) { loadReddit(data, $("#reddits")); });

};

function loadReddit(data, container)
{
    var json = JSON.parse(data);
    var subs = json.filter(function(sub) { return sub._topics.length; } )
                   .map(function(sub) {
                        var table = $("<table class='item reddit-item'><thead><tr><th><h1>"+ sub._name +"</h1></th></tr></thead></table>");
                        return table.append.apply( table, sub._topics.map(function(topic) {
                            return generateRedditView(sub._name, topic); }));
                     });

    container.append.apply(container, subs);

}

function loadWeather(data, container)
{
    var json = JSON.parse(data);
    var weathers = json.filter(function(city) { return city.forecasts.length; } )
                     .map(function(city) {
                         return city.forecasts.map(function(forecast) {
                             return generateWeatherView(city.city, forecast); });
                     });

    container.append.apply(container, weathers);
}

function loadAnimes(data, container)
{
    var json = JSON.parse(data);
    var animes = json.filter(function(anime) { return anime._episodes.length; } )
                     .sort(function(a,b) { return a._episodes[0]._date < b._episodes[0]._date;} )
                     .map(function(anime) {
                         return anime._episodes.map(function(episode) {
                             return generateAnimeView(anime, episode); });
                     });


    container.append.apply(container, animes);
}

function loadVideos(data, container)
{
    var json = JSON.parse(data);
    var videos = json.filter(function(channel) { return channel._videos.length; } )
                     .map(function(channel) {
                         return channel._videos.map(function(video) {
                             return generateVideoView(channel._name, video); });
                     });


    container.append.apply(container, videos);

}

function loadSeries(data, container)
{
    var json = JSON.parse(data);
    var series = json.filter(function(serie) { return serie._episodes.length} )
                     .sort(function(a,b) { return a._episodes[0]._date < b._episodes[0]._date;} )
                     .map(function(serie) {
                        return serie._episodes.map(function(episode) {
                            return generateSerieView (serie._name, episode);

                        });
                    });


    container.append.apply(container, series);

}


function generateRedditView(subName, topic)
{
    var generateThumbnail = function(topic) { 
        var url = topic._thumbnail;
        return (url.length && url != "self") ?  
            '<a href="'+ topic._url + '"><img style="vertical-align: middle;" src="' + url  + '"/></a>' 
            : "⇒"; 
    }

    
    var row = $('<tr></tr>');
    var cell = $('<th></th>');
    var row_bullet = $('<th class="bullet">'+ generateThumbnail(topic)+ '</th>');
    var link = $('<a href="' + topic._url + '"><b>'+ topic._title + '</b></a>' +
        '<br/><a style="font-size:12px;" href="'+ topic._commentLink + '"> ' + topic._numComments + ' comments </a>');

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
        $(".cd-panel-header-title").html(cityName);
        callAjax("/meteo/" + cityName, function(data) {
            var panel = $(".cd-panel-content");
            panel.empty();
            loadWeather(data, panel);
            panel.animate({ scrollTop: 0 }, 0);
            $('.cd-panel').addClass('is-visible');
        });
    });

    return container.append.apply(container, [header, thumbnail, footer]);
}

function generateAnimeView(anime, episode)
{
    var container = $("<div class='item anime-item'></div>");

    var header = $('<header>' +
                       '<a href=""><h1>' + anime._name + '</h1></a>' +
                   '</header>'
                  );

    var thumbnail = $('<div class="thumbnail">' +
                        '<a href="' + episode._magnetURI + '">' +
                        '<img src="' + anime._thumbnail + '" /></a>' +
                      '</div>'
                     );

    var date = new Date(episode._date * 1000);
    var footer = $('<footer>' +
                     '<a href="' + episode._magnetURI + '">' +
                         '<h3>' + episode._title + '</h3>' +
                         '<h3>' + date.toLocaleDateString() + '</h3>' +
                     '</a>' +
                   '</footer>');

    header.click(function(event) {
        event.preventDefault();
        $(".cd-panel-header-title").html(anime._name);
        callAjax("/anime/" + anime._name, function(data) {
            var panel = $(".cd-panel-content");
            panel.empty();
            loadAnimes(data, panel);
            panel.animate({ scrollTop: 0 }, 0);
            $('.cd-panel').addClass('is-visible');
        });
    });


    return container.append.apply(container, [header, thumbnail, footer]);
}

function generateSerieView(serieName, episode)
{
    var container = $("<div class='item serie-item'></div>");
    var header = $('<header>' +
                   '<a href=""><h1>' + serieName + '</h1></a>' +
                   '</header>');

    var date = new Date(episode._date * 1000);
    var footer = $('<footer class="footer">' +
                        '<a href="' + episode._magnetURI + '">' +
                            '<h3>' + episode._title + '</h3>' +
                            '<h3>' + date.toLocaleDateString() + '</h3>' +
                        '</a>' +
                    '</footer>'
                   );


    header.click(function(event) {
        event.preventDefault();
        $(".cd-panel-header-title").html(serieName);
        callAjax("/serie/" + serieName, function(data) {
            var panel = $(".cd-panel-content");
            panel.empty();
            loadSeries(data, panel);
            panel.animate({ scrollTop: 0 }, 0);
            $('.cd-panel').addClass('is-visible');
        });
    });


    return container.append.apply(container, [header, footer]);

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
                       '<a href="' + video._url + '">' +
                           '<img src="' + video._thumbnail + '"/>' +
                       '</a>' +
                     '</div>'
                    );

    var footer = $('<div class="footer">' +
                     '<a href="' + video._url + '">' +  video._title + '</a>' +
                   '</div>'
                 );


    header.click(function(event) {
        event.preventDefault();
        $(".cd-panel-header-title").html(channelName);
        callAjax("/video/" + channelName, function(data) {
            var panel = $(".cd-panel-content");
            panel.empty();
            loadVideos(data, panel);
            panel.animate({ scrollTop: 0 }, 0);
            $('.cd-panel').addClass('is-visible');
        });
    });


    return container.append.apply(container, [header, thumbnail, footer]);
}


function callAjax(url, callback)
{
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function(){
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200){
            callback(xmlhttp.responseText);
        }
    }
    xmlhttp.open("GET", url, true);
    xmlhttp.send();
}

