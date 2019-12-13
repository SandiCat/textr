FROM fpco/stack-build:lts-14.17 as build

VOLUME /opt/build/stack-artefacts
# ^ this should be created automatically

RUN mkdir -p /opt/build/src
COPY . /opt/build/src/

WORKDIR /opt/build/src

#RUN --mount=type=bind,src=~/.stack,destination=/opt/build/stack-root/ stack build --system-ghc --stack-root /opt/build/stack-root --work-dir /opt/build/stack-work
#RUN --mount=target=~/.stack,type=cache stack build --system-ghc --stack-root /opt/build/stack-root --work-dir /opt/build/stack-work
RUN stack build --system-ghc \
    --stack-root /opt/build/stack-artefacts/stack-root \
    --work-dir /opt/build/stack-artefacts/stack-work

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------

FROM fpco/stack-build-small:lts-14.17 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY --from=build /opt/build/bin .

EXPOSE 80
CMD ["/opt/app/server"]