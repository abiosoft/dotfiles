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
    clang

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
    ffmpeg
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
    jdk
    rustup
    dotnet-sdk

    # container/devops
    docker-client
    docker-compose_2
    docker-buildx
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

    # others
    _1password
   ];

  # macOS specific packages
  macosPackages = with pkgs; [
    mas
  ];

  # Linux specific packages
  linuxPackages = with pkgs; [
    coreutils
  ];

  # zsh theme
  zshThemeFile = builtins.readFile "${homeDir}/dotfiles/zsh/abiola.zsh-theme";
  customOhMyZshDir = pkgs.stdenv.mkDerivation {
    name = "oh-my-zsh-custom-dir";
    src = builtins.toFile "abiola.zsh-theme" "${zshThemeFile}";
    phases = [ "buildPhase" ];
    buildPhase = ''
      mkdir -p $out/themes
      cp $src $out/themes/abiola.zsh-theme
    '';
  };

  # bat theme
  batTheme = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "chriskempson";
    repo = "tomorrow-theme";
    rev = "de38ebc802bdc611c4404b5cd8db941dd6d2c171";
    sha256 = "sha256-9FDxDCObULfKGUDdvGTsbQMz+QSmGxG6e3IVaweUegA=";
  } + "/textmate/Tomorrow-Night.tmTheme");

  # dotnet env var
  dotnetRoot = "${pkgs.dotnet-sdk}";

  extraPaths = builtins.concatStringsSep ":" [
    "${homeDir}/bin"
    "${homeDir}/dotfiles/bin"

    # dotnet
    "${homeDir}/dotnet/tools"
  ];

in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = homeDir;

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

  # allow unfree packages
  nixpkgs.config.allowUnfree = true;

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
      export PATH="$PATH:${extraPaths}"

      # dotnet tools
      export DOTNET_ROOT=${dotnetRoot}

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

  # bat
  programs.bat = {
    enable = true;
    themes = {
      tomorrow-night = "${batTheme}";
    };
    config = {
      theme = "tomorrow-night";
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
