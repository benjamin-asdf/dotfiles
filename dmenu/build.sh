#!/bin/sh
# Downloads dmenu 5.4, applies wrap-around patch, builds, and installs
# binary to scripts/.local/bin/pathoverride/dmenu-bin
set -e

DMENU_VERSION="5.4"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BUILD_DIR="$SCRIPT_DIR/dmenu-${DMENU_VERSION}"
INSTALL_DIR="$SCRIPT_DIR/../scripts/.local/bin/pathoverride"

# Clean previous build
rm -rf "$BUILD_DIR"

# Download and extract
echo "Downloading dmenu ${DMENU_VERSION}..."
curl -sL "https://dl.suckless.org/tools/dmenu-${DMENU_VERSION}.tar.gz" | tar xz -C "$SCRIPT_DIR"

# Apply wrap-around patch
echo "Applying wrap-around patch..."
cd "$BUILD_DIR"
patch -p1 < "$SCRIPT_DIR/wrap-around.diff"

# Build
echo "Building..."
make

# Install
echo "Installing to $INSTALL_DIR/dmenu-bin..."
cp dmenu "$INSTALL_DIR/dmenu-bin"
chmod +x "$INSTALL_DIR/dmenu-bin"

# Clean up
echo "Cleaning up build directory..."
cd "$SCRIPT_DIR"
rm -rf "$BUILD_DIR"

echo "Done."
