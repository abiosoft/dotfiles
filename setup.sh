#!/usr/bin/env bash


dirs=(
    "neovim"
    "tmux"
    "zsh"
)


if [ -z "$distro" ]; then
    echo distro not specified, required.
    exit 1
fi

if [ -z "$desktop" ]; then
    echo desktop not specified, omitting desktop
else
    dirs+=( $desktop )
fi

echo "setting up for $distro with $desktop..."

for dir in "${dirs[@]}"; do
    echo setting up "$dir"...
    sh <<EOF
    cd $dir
    if [ -f deps-$distro.sh ]; then
        ./deps-$distro.sh
    else
        echo no deps for $dir
    fi

    [ $? -eq 0 ] && ./install.sh
EOF

    if [ $? -ne 0 ]; then
        exit 1
    fi

    echo "done with $dir".
done
