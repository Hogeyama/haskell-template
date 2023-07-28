#!/usr/bin/env bash
set -euxo pipefail
# Usage:
# Run `arion up -d webserver` before running this script.

# Input
: HOST="${HOST:=localhost:3000}"

output=$(curl -sSf "http://$HOST/hello/world")
if [[ "$output" = "Hello world!" ]]; then
  echo "Found expected output 'Hello world!'"
else
  echo "Unexpected output '$output'"
  exit 1
fi
