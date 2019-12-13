FROM fpco/stack-build:lts-14.17 as build

VOLUME /opt/build/
# ^ this should be created automatically

RUN mkdir -p /opt/build/src/

# TODO: use another volume for .stack-work so that it can be shared between containers and mounted from host
# TODO: rm everything but .stack-work in case there's some file fuckery
# TODO: does .dockerignore apply to COPY?

COPY . /opt/build/src/

WORKDIR /opt/build/src
