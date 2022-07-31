# helpers
nix-path() (
  nix-env -q --out-path ${1} 2>/dev/null | awk -F' ' '{print $2}'
)

# ZSH
export ZSH="$(nix-path oh-my-zsh)/share/oh-my-zsh"

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
alias nix-switch='nix-env -if ~/.packages.nix'

# git commit editor
export VISUAL=vim
export EDITOR="$VISUAL"

# GO
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin:$HOME/dotfiles/bin"

# dotNet
export PATH="$PATH:$HOME/.dotnet/tools"
export DOTNET_ROOT="$(nix-path dotnet-sdk)"

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
