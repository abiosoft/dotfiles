{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "abiola";
  home.homeDirectory = "/Users/abiola";

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
  home.packages = with pkgs; [
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

    # internet
    youtube-dl
    wget
    axel
    curl

    # programming/sdks
    python3
    go_1_17
    nodejs-16_x
    yarn
    jdk11
    rustup

    # container/vm/devops
    docker-client
    kubectl
    kubectx
    kind
    terraform
    pulumi-bin
    vault
    google-cloud-sdk
  ];

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
}
