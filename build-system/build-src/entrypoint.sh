#!/bin/bash

./_build/bin/server &
nginx -c /data/build-src/nginx.conf