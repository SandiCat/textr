# supposed to be an exact copy of the production Dockerfile
# except that directories are mounted instead of copied
# to shorten the feedback cycle
FROM fpco/stack-build-small:lts-14.17

RUN apt-get update
RUN apt-get -y install nginx libpq-dev

RUN mkdir /data
WORKDIR /data

# mount these instead
# COPY build-src/ /data/build-src/
# COPY _build/ /data/_build/

EXPOSE 80
CMD ["/data/build-src/entrypoint.sh"]
