FROM alpine
RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY ./_build/bin /opt/app

EXPOSE 80
CMD ["/opt/app/server"]