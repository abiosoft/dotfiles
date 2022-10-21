/*
  create a copy as packages.nix and edit accordingly

  cp packages.sample.nix packages.nix
*/
(
  with import <nixpkgs-unstable> { };
  [
    go_1_19
  ]
)
++
(
  with import <nixpkgs> { };
  [
    ## things fail to build without these
    automake
    autoconf
    autoconf-archive
    pkg-config
    gnumake
    cmake
    libtool
    clang
    ctags

    ## programming/sdks
    python310
    ruby_3_1
    nodejs-18_x
    yarn
    jdk
    rustup
    dotnet-sdk
    postgresql

    ## container/devops
    docker-buildx
    docker-credential-gcr
    docker-credential-helpers
    buildkit
    # kubectx
    kubernetes-helm
    kind
    # terraform
    # vault
    buildpack
    google-cloud-sdk
    cloud-sql-proxy
  ]
)
