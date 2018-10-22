#!/usr/bin/env bash

export LC_ALL=en_US.UTF-8

while true; do
    echo Battery: $(acpi -V | head -n1 | awk -F':' '{print $2}' | awk -F', ' '{print $2}') '|' $(date +'%Y-%m-%d %I:%M%p ')
    sleep 5
done

