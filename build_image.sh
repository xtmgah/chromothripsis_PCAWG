#!/usr/bin/env bash
set -e

REPO=scottx611x/chromothripsis-pcawg

export STAMP=`date +"%Y-%m-%d_%H-%M-%S"`

sudo docker login -u $DOCKER_USER -p $DOCKER_PASS
docker pull $REPO # Defaults to "latest", but just speeds up the build, so precise version doesn't matter.
docker build --cache-from $REPO \
             --tag image-$STAMP \
             .
