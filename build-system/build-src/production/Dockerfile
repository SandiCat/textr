FROM fpco/stack-build-small:lts-14.17

RUN apt-get update
RUN apt-get -y install nginx

RUN mkdir -p /data
WORKDIR /data

COPY build-src/production/entrypoint.sh /data/entrypoint.sh
COPY build-src/nginx.conf /data/nginx.conf
COPY _build/bin/server /data/server

EXPOSE 80
CMD ["/data/entrypoint.sh"]
