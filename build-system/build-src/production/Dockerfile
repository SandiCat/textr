FROM fpco/stack-build-small:lts-14.17

RUN apt-get update
RUN apt-get -y install nginx libpq-dev

RUN mkdir /data
WORKDIR /data

COPY build-src/ /data/build-src/
COPY _build/production/ /data/_build/

EXPOSE 80
CMD ["/data/build-src/entrypoint.sh"]
