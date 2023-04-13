# create a copy as packages.sh and edit accordingly
#   cp packages.sample.sh packages.sh

unstable_packages=(
    "go_1_20"
    "qemu"
)

stable_packages=(
    ## things fail to build without these
    "automake"
    "autoconf"
    "autoconf-archive"
    "pkg-config"
    "gnumake"
    "cmake"
    "libtool"
    "ctags"
    "gcc"

    ## programming/sdks
    "python311"
    "ruby_3_1"
    "nodejs-19_x"
    "yarn"
    "jdk"
    "rustup"
    "dotnet-sdk_7"
    "postgresql"

    ## container/devops
    "docker-buildx"
    "docker-credential-gcr"
    "docker-credential-helpers"
    "buildkit"
    "kubectx"
    "kubernetes-helm"
    "kind"
    "terraform"
    "vault"
    "buildpack"
    "google-cloud-sdk"
    "cloud-sql-proxy"

    # others
    "caddy"
    "norouter"
)
