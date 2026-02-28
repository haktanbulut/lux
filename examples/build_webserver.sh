#!/bin/bash
# Build the Lux coroutine web server
# Usage: ./examples/build_webserver.sh
# Then run: ./webserver

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SCRIPT_DIR")"

echo "==> Compiling C socket helper..."
gcc -Wall -Wextra -std=c11 -O2 -c \
    "$SCRIPT_DIR/webserver_helper.c" \
    -o /tmp/webserver_helper.o

echo "==> Compiling webserver.lux..."
"$ROOT/lux" "$SCRIPT_DIR/webserver.lux" 2>/dev/null || true

echo "==> Linking..."
cc /tmp/lux_output.o /tmp/lux_runtime.o /tmp/webserver_helper.o \
    -o "$ROOT/webserver"

echo "==> Done!"
echo "    Run:  ./webserver"
echo "    Test: curl http://localhost:8080/"
echo "          curl http://localhost:8080/hello"
echo "          curl http://localhost:8080/status"
