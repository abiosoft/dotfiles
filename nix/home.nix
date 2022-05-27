{ config, pkgs, ... }:

let
  user = builtins.getEnv "USER";
  isMacOS = builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin";

  # cross-platform packages
  packages = with pkgs; [
    # things fail to build without these
    automake
    autoconf
    autoconf-archive
    pkg-config
    gnumake
    cmake
    libtool

    # utils
    bat
    zsh
    jq
    yq
    tree
    htop
    fzf
    watch
    ripgrep
    silver-searcher
    git
    gnupg
    coreutils
    nettools
    gh
    nixpkgs-fmt
    # mkdocs breaking for whatever reason

    # internet
    youtube-dl
    wget
    axel
    curl
    speedtest-cli

    # programming/sdks
    python310
    ruby
    go_1_18
    nodejs-18_x
    yarn
    jdk11
    rustup

    # container/devops
    docker-client
    docker-compose_2
    docker-credential-gcr
    docker-credential-helpers
    buildkit
    kubectl
    kubectx
    kubernetes-helm
    kind
    terraform
    pulumi-bin
    vault
    packer
    buildpack
    google-cloud-sdk

    # virtualization
    vagrant
    qemu
  ];

  # macOS specific packages
  macosPackages = with pkgs; [
    mas
  ];

  # Linux specific packages
  linuxPackages = with pkgs; [
    dotnet-sdk
  ];

in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = if isMacOS then "/Users/${user}" else "/home/${user}";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # packages
  home.packages =
    if isMacOS
    then packages ++ macosPackages
    else packages ++ linuxPackages;

  # vim
  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraConfig = "source ~/dotfiles/neovim/init.vim";
    plugins = with pkgs.vimPlugins; [
      vim-nix
    ];
  };

  # tmux
  programs.tmux = {
    enable = true;
    extraConfig = "source-file ~/dotfiles/tmux/tmux.conf";
  };

  # utils
  programs.command-not-found.enable = true;
}
