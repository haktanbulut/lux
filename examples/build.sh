#!/bin/bash
# Build the raylib cube demo
# Usage: ./examples/build.sh
# Requires: raylib installed (pacman -S raylib)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SCRIPT_DIR")"

echo "==> Compiling raylib wrapper..."
gcc -Wall -Wextra -std=c11 -O2 -c \
    "$SCRIPT_DIR/raylib_wrapper.c" \
    -o /tmp/raylib_wrapper.o

echo "==> Compiling cube.lux to object file..."
# lux will emit /tmp/lux_output.o then try to link (which will fail due to
# missing wrapper symbols). We just need the .o, so ignore the link error.
"$ROOT/lux" "$SCRIPT_DIR/cube.lux" 2>/dev/null || true

echo "==> Linking everything with raylib..."
cc /tmp/lux_output.o /tmp/lux_runtime.o /tmp/raylib_wrapper.o \
    -lraylib -lGL -lm -lpthread -ldl -lrt \
    -o "$ROOT/cube_demo"

echo "==> Done! Run: ./cube_demo"
