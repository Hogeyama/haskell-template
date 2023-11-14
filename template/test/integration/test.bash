#!/usr/bin/env bash
set -euxo pipefail

# Input
: "${HOST:=localhost:3000}"

output=$(curl -sSf "http://$HOST/hello/world")
if [[ "$output" = "Hello world!" ]]; then
  echo "Found expected output 'Hello world!'"
else
  echo "Unexpected output '$output'"
  exit 1
fi
