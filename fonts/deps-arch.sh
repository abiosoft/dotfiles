#!/usr/bin/env bash

echo installing deps...
sudo pacman -Sy --noconfirm fontconfig
if [ $? -ne 0 ]; then
    exit 1
fi

