# helpers
nix-path() (
  nix-env -q --out-path ${1} 2>/dev/null | awk -F' ' '{print $2}'
)

# ZSH
if [ -z "$HOMEBREW_PREFIX" ]; then
  export ZSH="$(nix-path oh-my-zsh)/share/oh-my-zsh"
else
  export ZSH="$HOME/.oh-my-zsh"
fi

ZSH_CUSTOM="$HOME/.config/my-zsh"
ZSH_THEME="abiola"
CASE_SENSITIVE="true"

plugins=(
  git
  command-not-found
  docker
  docker-compose
  kubectl
)

[ -f $ZSH/oh-my-zsh.sh ] && source $ZSH/oh-my-zsh.sh

# personal scripts
export PATH="$PATH:$HOME/bin"

# aliases
alias vim='nvim'
alias krun='kubectl run --namespace default --restart=Never -it --rm tmpbox --image'
alias brew-switch='brew bundle -v --file ~/.config/brew/Brewfile && brew bundle --force cleanup --file ~/.config/brew/Brewfile'
alias nix-switch='nix-env -irf ~/.config/nix/core.nix --log-format bar'
alias colima-shell='nix-shell -p $(nix-build ~/projects/golang/colima)'

# git commit editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# GO
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin:$HOME/dotfiles/bin"

# Rust
export PATH="$PATH:$HOME/.cargo/bin"

# dotNet
export PATH="$PATH:$HOME/.dotnet/tools"
if [ -z "$HOMEBREW_PREFIX" ]; then
  export DOTNET_ROOT="$(nix-path dotnet-sdk)"
else
  export DOTNET_ROOT="$HOMEBREW_PREFIX/opt/dotnet/libexec"
fi

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# use python 3 by default
alias python='python3'

# gpg
export GPG_TTY=$(tty)

# bat
alias cat='bat'
export BAT_THEME="tomorrow-night"
export PAGER="bat"

# mac VMs
if uname -a | grep VMAPPLE >/dev/null; then
  DOCKER_HOST="$(ifconfig en0 | grep "inet " | awk -F' ' '{print $2}' | awk -F'.' '{print $1"."$2"."$3".1"}')"
  export DOCKER_HOST="tcp://$DOCKER_HOST:5100"
fi
