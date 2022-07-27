{ pkgs }:

with pkgs; [
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

  ## utils
  # bat
  # tmux
  # zsh
  jq
  tree
  htop
  # fzf
  # watch
  # ripgrep
  # silver-searcher
  # shellcheck
  # git
  # gnupg
  # nettools
  # gh
  # ffmpeg
  # mkdocs breaking for whatever reason

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
  # jdk
  # rustup
  dotnet-sdk
  postgresql

  ## container/devops
  docker-client
  docker-compose
  docker-buildx
  docker-credential-gcr
  docker-credential-helpers
  # buildkit
  kubectl
  # kubectx
  kubernetes-helm
  # kind
  # terraform
  # vault
  # buildpack
  google-cloud-sdk
]
