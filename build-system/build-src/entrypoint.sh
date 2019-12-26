#!/bin/bash

./_build/bin/server /data/_build/config/server-config.json &
nginx -c /data/build-src/nginx.conf