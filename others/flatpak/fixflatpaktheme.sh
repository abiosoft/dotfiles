#!/usr/bin/env bash

for dir in $HOME/.var/app/*/
do
    confdir="${dir}config/gtk-3.0"
    mkdir -p $confdir
    cp $HOME/.config/gtk-3.0/settings.ini $confdir/settings.ini
done
