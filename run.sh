#!/bin/bash

set -e
set -x

cd /app
sudo -u nobody -E env "PATH=$PATH" /app/soh-test
