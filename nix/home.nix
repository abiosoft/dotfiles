{ config, pkgs, ... }:

let
  user = builtins.getEnv "USER";
  homeDir = builtins.getEnv "HOME";
  isMacOS = builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin";
  link = config.lib.file.mkOutOfStoreSymlink;
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
    dotnet-sdk

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
    coreutils
  ];

  customOhMyZshDir = pkgs.stdenv.mkDerivation {
    name = "oh-my-zsh-custom-dir";
    phases = [ "buildPhase" ];
    buildPhase = ''
      mkdir -p $out/themes
      cp ${homeDir}/dotfiles/zsh/abiola.zsh-theme $out/themes/abiola.zsh-theme
    '';
  };

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

  # utils
  programs.command-not-found.enable = true;

  # vim
  home.file.".config/nvim/colors/tomorrow.vim".source = link "${homeDir}/dotfiles/neovim/tomorrow.vim";
  home.file.".config/nvim/colors/tomorrow-night.vim".source = link "${homeDir}/dotfiles/neovim/tomorrow-night.vim";
  home.file.".config/nvim/coc-settings.json".source = link "${homeDir}/dotfiles/neovim/coc-settings.json";
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

  # zsh
  programs.zsh = {
    enable = true;
    autocd = false;
    shellAliases = {
      krun = "kubectl run --namespace default --restart=Never -it --rm tmpbox --image";
      nerd = "nerdctl";
      c = "colima";
      ls = "ls --color=auto";
    };
    initExtra = ''
      # private bin
      export PATH="$PATH:$HOME/bin"

      # dotfiles
      export PATH="$PATH:$HOME/dotfiles/bin"

      # dotnet tools
      export PATH="$PATH:$HOME/.dotnet/tools"

      # git commit editor
      export VISUAL=vim
      export EDITOR="$VISUAL"
    '';
    oh-my-zsh = {
      enable = true;
      theme = "abiola";
      custom = "${customOhMyZshDir}";
      plugins = [
        "git"
        "command-not-found"
        "docker"
        "docker-compose"
        "kubectl"
      ];
      extraConfig = ''
        CASE_SENSITIVE="true"
      '';
    };
  };

  programs.git = {
    enable = true;
    userName = "Abiola Ibrahim";
    userEmail = "git@abiosoft.com";
    lfs.enable = true;
    extraConfig = ''
      [url "ssh://git@github.com/abiosoft/"]
        insteadOf = https://github.com/abiosoft/
      [core]
        autocrlf = input
      [github]
        user = abiosoft
    '';
  };

}
