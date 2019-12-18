FROM fpco/stack-build-small:lts-14.17

RUN apt-get update
RUN apt-get -y install nginx

RUN mkdir /data
WORKDIR /data

COPY build-src/ /data/build-src/
COPY _build/ /data/_build/

EXPOSE 80
CMD ["/data/build-src/entrypoint.sh"]
