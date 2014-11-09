jQuery(document).ready(function($){
    var contentSections = $('.cd-section'),
        navigationItems = $('#cd-vertical-nav a');

    updateNavigation();
    $(window).on('scroll', function(){
        updateNavigation();
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

        callAjax("/video/", loadVideos);
        callAjax("/serie/", loadSeries);
        callAjax("/anime/", loadAnimes);

};

function loadAnimes(data)
{
    var json = JSON.parse(data);
    str = json.filter(function(anime) { return anime._episodes.length} )
              .map(function(anime) { return generateAnimeView(anime, anime._episodes[0]); })
              .join("");

    $("#animes").append(str);
}

function loadVideos(data)
{
    var json = JSON.parse(data);
    str = json.filter(function(channel) { return channel._videos.length} )
              .map(function(channel) { return generateVideoView(channel._name, channel._videos[0]); })
              .join("");

   $("#videos").append(str);
}

function loadSeries(data)
{
    var json = JSON.parse(data);

    json.filter(function(serie) { return serie._episodes.length} )
        .forEach(function(serie) {
            serie._episodes.forEach(function(episode) {
                var html = generateSerieView (serie._serieName, episode);
                $("#series").append(html);

            });
    });



}

function generateAnimeView(anime, episode)
{
    var str = '';
    str += '<div class="itemAnime">';
        str += '<div class="header" style="height: 10%">';
        str += '<a><h4 style="margin:0px;">' + anime._title.capitalize() + '</h4></a>';
        str += "</div>";

        str += '<div class="thumbnail" style="height:60%; width:60%;">';
        str += '<a href="' + episode._magnetURI + '">';
        str += '<img src="' + anime._thumbnail + '" /></a>';
        str += '</div>';


        str += '<div class="footer" style="height:30%;">';
        str += '<a href="' + episode._magnetURI + '">';
        str += '<p style="margin:0px;"><b>' + episode._name + '</b></p>';
        str += '<p style="margin:0px;">' + new Date(episode._date * 1000) + '</p>';
        str += '</a>';
        str += '</div>';
    str += '</div>';

    return str;
}

function generateSerieView(serieName, episode)
{
        var header = '';
        header += '<div class="header dropdown" style="height:30%">';
        header += '<a id="dLabel" type="button" style="width: 90%;" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" >';
        header += '<h3>' + serieName.capitalize() + '<span class="caret"></span></h3></a>';
        header += '<ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">';
        header += '</ul>';
        header += "</div>";
        header = $(header);


        header.click(function() {
            var menu = header.find("ul");
            if(menu.children().length) return;

            callAjax("/serie/" + serieName, function(data) {
                menu.append(fillEpisodesDropdown(data));
            });
        });

        var footer = '';
        footer += '<div class="footer" style="height:70%;">';
        footer += '<a href="' + episode._magnetURI + '">';
        footer += '<p style="margin:0px;">' + episode._name + '</p>';
        footer += '<p style="margin:0px;">' + episode._date + '</p>';
        footer += '</a>';
        footer += '</div>';
        footer = $(footer);

    var str = '';
    str += '<div class="itemSerie">';
    str += '</div>';
    return $(str).append(header).append(footer);

}

function fillEpisodesDropdown(data)
{
    var json = JSON.parse(data);

    var episodes =  json.map(function(serie) {
               return serie._episodes.map(function(episode) {
                   return '<li><a role="menuitem" href="' + episode._magnetURI + '"><b>' + episode._name + '</b> ' + episode._date + '</a></li>';
              });

    });


    return [].concat.apply([], episodes);

}


function generateVideoView(channelName, video)
{
    var str = '';
    str += '<div class="item videoItem">';
        str += '<div class="header">';
        str += '<a href="' + "https://www.youtube.com/user/" + channelName + "/videos" + '"><h1>' + channelName + '</h1></a>';
        str += "</div>";

        str += '<div class="thumbnail">';
        str += '<a href="' + video._url + '">';
        str += '<img src="' + video._thumbnail + '"/></a>';
        str += '</div>';


        str += '<div class="footer">';
        str += '<a href="' + video._url + '">' +  video._titre + '</a>';
        str += '</div>';
    str += '</div>';

    return str;
}


String.prototype.capitalize = function() {
    return this.charAt(0).toUpperCase() + this.slice(1);
}

function callAjax(url, callback)
{
    var xmlhttp;
    // compatible with IE7+, Firefox, Chrome, Opera, Safari
    xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function(){
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200){
            callback(xmlhttp.responseText);
        }
    }
    xmlhttp.open("GET", url, true);
    xmlhttp.send();
}

