set dir /data/code/haskell/timesafe

cd $dir/build-system/build-src/dev-host/
docker-compose up &

cd $dir/timesafe-backend/
ghcid -c "stack repl --main-is :server" \
    --no-height-limit \
    --reload app/ \
    --reload src/ \
    -T Main.develMain &

cd $dir/timesafe-frontend/
yarn parcel src/index.html &

devd \
    /=http://localhost:1234 \
    /api/=http://localhost:8080/api/ \
    /static/=./build-system/_build/development/static/

kill (jobs -p)[1..1000]