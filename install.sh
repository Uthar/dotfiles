#!/bin/sh -e

echo "installing..."
mkdir -pv ~/.config
stow -R gnu
