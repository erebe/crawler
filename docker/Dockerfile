FROM alpine:latest
MAINTAINER github@erebe.eu


RUN adduser -D crawler
ADD https://github.com/erebe/crawler/releases/download/1.1/crawler.tar.gz?raw=true /home/crawler/
ADD run.sh /home/crawler/
RUN cd /home/crawler && \
    tar xzvf crawler.tar.gz && \
    rm crawler.tar.gz

USER crawler
VOLUME /data
WORKDIR /home/crawler

ENV SLEEP_TIME_SEC 1800

CMD ["./run.sh"]

