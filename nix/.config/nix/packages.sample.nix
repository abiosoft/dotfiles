/*
  create a copy as packages.nix and edit accordingly

  cp packages.sample.nix packages.nix
*/

{ nixpkgs ? import <nixpkgs> { }, nixpkgs-unstable ? import <nixpkgs-unstable> { } }:

(
  with nixpkgs-unstable;
  [
    go
    qemu
  ]
)
++
(
  with nixpkgs;
  [
    ## things fail to build without these
    automake
    autoconf
    autoconf-archive
    pkg-config
    gnumake
    cmake
    libtool
    ctags
    gcc

    ## programming/sdks
    python312
    ruby_3_1
    nodejs_21
    yarn
    jdk
    rustup
    dotnet-sdk_7
    postgresql

    ## container/devops
    docker-buildx
    docker-credential-gcr
    docker-credential-helpers
    buildkit
    kubectx
    kubernetes-helm
    kind
    terraform
    vault
    buildpack
    google-cloud-sdk
    google-cloud-sql-proxy

    # others
    caddy
    norouter
  ]
)
