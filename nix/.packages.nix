let
  pkgs = import <nixpkgs> { };
in
with pkgs ; [
  ### uncomment as needed

  ## nix
  nixpkgs-fmt

  ## things fail to build without these
  # automake
  # autoconf
  # autoconf-archive
  # pkg-config
  # gnumake
  # cmake
  # libtool
  # clang
  # ctags

  ## shell
  stow
  bat
  tmux
  zsh
  neovim
  oh-my-zsh
  tree

  ## utils
  jq
  htop
  fzf
  watch
  ripgrep
  silver-searcher
  shellcheck
  git
  delta
  gh
  gnupg
  mkdocs

  ## internet
  # youtube-dl
  # wget
  # axel
  # curl
  # speedtest-cli

  ## programming/sdks
  python310
  ruby
  go_1_18
  nodejs-18_x
  yarn
  jdk
  rustup
  dotnet-sdk
  postgresql

  ## container/devops
  docker-client
  docker-compose
  docker-buildx
  docker-credential-gcr
  docker-credential-helpers
  buildkit
  kubectl
  # kubectx
  kubernetes-helm
  kind
  terraform
  vault
  buildpack
  google-cloud-sdk
]

