#!/usr/bin/env bash
set -e

REPO=scottx611x/chromothripsis-pcawg

export STAMP=`date +"%Y-%m-%d_%H-%M-%S"`
export SUFFIX=-standalone
echo "image-$STAMP"

CONTAINER_NAME="container-$STAMP$SUFFIX"
mkdir "/tmp/$CONTAINER_NAME"

sudo docker login -u $DOCKER_USER -p $DOCKER_PASS

docker pull $REPO # Defaults to "latest", but just speeds up the build, so precise version doesn't matter.
docker build --cache-from $REPO \
             --tag image-$STAMP \
             .
docker run --name $CONTAINER_NAME \
           --detach \
           -p 3242:3242 \
           image-$STAMP
rm -r "/tmp/$CONTAINER_NAME"