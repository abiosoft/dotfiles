# ZSH
if [ -z "$HOMEBREW_PREFIX" ]; then
  export ZSH="$HOME/.nix-profile/share/oh-my-zsh"
else
  export ZSH="$HOME/.oh-my-zsh"
  export HOMEBREW_NO_AUTO_UPDATE=1
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
alias brew-switch='brew bundle install -v --cleanup --file ~/.config/brew/Brewfile'
alias nix-switch='DRV="$(nix build path:$HOME/dotfiles/nix/.config/nix/ --print-out-paths --no-link)" && nix profile remove ".*" 2>/dev/null && nix profile install "$DRV"'
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
  [ -f $HOME/.nix-profile/bin/dotnet ] && export DOTNET_ROOT="$(dirname $(realpath $HOME/.nix-profile/bin/dotnet))"
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

# nix shell
if echo $PATH | grep -q "/nix/store"; then
  export IN_NIX_SHELL="true"
fi

# mac VMs
if uname -a | grep VMAPPLE >/dev/null; then
  export DOCKER_HOST="ssh://colima"
fi
