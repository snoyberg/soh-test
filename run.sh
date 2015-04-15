#!/bin/bash

set -e
set -x

cd /app
sudo -u nobody -E /app/soh-test
