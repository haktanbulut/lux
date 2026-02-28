#!/bin/bash
# Build the Lux Asteroids game
# Usage: ./examples/build_asteroids.sh
# Requires: raylib installed (pacman -S raylib)

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(dirname "$SCRIPT_DIR")"

echo "==> Compiling raylib wrapper..."
gcc -Wall -Wextra -std=c11 -O2 -c \
    "$SCRIPT_DIR/raylib_wrapper.c" \
    -o /tmp/raylib_wrapper.o

echo "==> Compiling asteroids.lux..."
"$ROOT/lux" "$SCRIPT_DIR/asteroids.lux" 2>/dev/null || true

echo "==> Linking with raylib..."
cc /tmp/lux_output.o /tmp/lux_runtime.o /tmp/raylib_wrapper.o \
    -lraylib -lGL -lm -lpthread -ldl -lrt \
    -o "$ROOT/asteroids"

echo "==> Done! Run: ./asteroids"
