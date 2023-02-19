let
  homeDir = builtins.getEnv "HOME";
in
let
  hasPackages = builtins.pathExists "${homeDir}/.config/nix/packages.nix";
in
(
  if hasPackages then (import "${homeDir}/.config/nix/packages.nix") else [ ]
)
++
(
  with import <nixpkgs> { };
  # core packages always required
  [
    ## nix
    nixpkgs-fmt

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
    gitFull
    delta
    gh
    pass
    gnupg
    shellcheck
    sqlite

    ## internet
    youtube-dl
    wget
    axel
    curl
    speedtest-cli

    ## container/devops
    docker-client
    docker-compose
    kubectl
  ]
)

/*
  For more packages, create a ~/.config/nix/packages.nix and populate accordingly with the packages

  with import <nixpkgs> { };
  [
  go_1_19
  python310
  ruby_3_1
  nodejs-18_x
  yarn
  jdk
  rustup
  dotnet-sdk
  postgresql
  ]

*/
