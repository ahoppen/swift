#!/bin/bash

set -e

BUILD_DIR="$1"
STDLIB_DIR="/workspaces/build/canonical/swift-linux-x86_64"

mkdir -p "$BUILD_DIR/lib"
mkdir -p "$BUILD_DIR/include"

if ! [[ -L "$BUILD_DIR/lib/swift" ]]; then
    ln -sf "$STDLIB_DIR/lib/swift" "$BUILD_DIR/lib/swift"
fi
if ! [[ -L "$BUILD_DIR/include/swift" ]]; then
    ln -sf "$STDLIB_DIR/include/swift" "$BUILD_DIR/include/swift"
fi