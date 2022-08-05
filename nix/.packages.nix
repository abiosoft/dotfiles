let
  homeDir = builtins.getEnv "HOME";
  pkgs = import <nixpkgs> { };
in
## uncomment after cloning source as I am actively working on the source.
  # let colima = import "${homeDir}/projects/golang/colima"; in [ colima ] ++
(
  with pkgs;
  [
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
    git
    delta
    # gh
    pass # used by docker-compose on Linux
    # gnupg
    # mkdocs
    # shellcheck

    ## internet
    # youtube-dl
    # wget
    # axel
    # curl
    # speedtest-cli

    ## programming/sdks
    # python310
    # ruby
    go_1_18
    nodejs-18_x
    yarn
    # jdk
    # rustup
    # dotnet-sdk
    # postgresql

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
    # kind
    # terraform
    # vault
    # buildpack
    google-cloud-sdk
    cloud-sql-proxy
  ]
)

