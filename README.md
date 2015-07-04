# Personnal Crawler

I use this programme to provide me with REST api/website homepage for the various sites that I read
Crawl every x min for the informations
Launching the program will create a webserver running on port 8086, I use it in synergy
with nginx to serve the homepage from my server

## What to expect:
- track torrents (ShowRss)
- track Youtube channel (API v3)
- track SubReddit 
- track weather (from openweathermap)
- track anime (from haruhichan)
- track Twitter (directly in the homepage)

## What to expect next (maybe):
- Add a one shot mode (for an usage with cron)
- Integration with Systemd
- tracking of looking for group
- tracking of a 4chan thread

## How to run ? 
##### To install (only once):
1. cabal sandbox init
2. cabal install
3. cp config/crawler.rc ~/.config/

##### Now to run: 
1. .cabal-sandbox/bin/crawler
2. Open yout browser to localhost:8086
3. Enjoy !

Example with the provided homepage
![alt text][logo]
[logo]: https://github.com/erebe/crawler/raw/master/example.png "Homepage Example"
