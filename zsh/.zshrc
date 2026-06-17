# ZSH
CUSTOM_ZSH="$HOME/.oh-my-zsh"
if [ -d "$CUSTOM_ZSH" ]; then
  export ZSH="$CUSTOM_ZSH"
fi

ZSH_CUSTOM="$HOME/.config/my-zsh"
ZSH_THEME="abiola"
CASE_SENSITIVE="true"

plugins=(
  git
  docker
  docker-compose
  kubectl
)

[ -f $ZSH/oh-my-zsh.sh ] && source $ZSH/oh-my-zsh.sh

# brew
if [ -n "$HOMEBREW_PREFIX" ]; then
  export HOMEBREW_NO_AUTO_UPDATE=1
  export HOMEBREW_NO_REQUIRE_TAP_TRUST=1
fi

# personal scripts
export PATH="$PATH:$HOME/bin"

# aliases
alias vim='nvim'
alias krun='kubectl run --namespace default --restart=Never -it --rm tmpbox --image'
alias brew-switch='brew bundle install -v --force --force-cleanup --file ~/.config/brew/Brewfile'
alias apt-switch='bash ~/.config/my-apt/core.sh'
alias dnf-switch='bash ~/.config/my-dnf/core.sh'
alias hugo-new='hugo new content "content/posts/$(date +%s)_newpost.md"'
[[ "$OSTYPE" == "darwin"* ]] && alias c='container'

# git commit editor
export VISUAL=nvim
export EDITOR="$VISUAL"

# GO
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin:$HOME/dotfiles/bin"

# Rust
export PATH="$PATH:$HOME/.cargo/bin"

# dotNet
export DOTNET_ROOT="$HOME/.dotnet"
export PATH="$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools"
if [ -n "$HOMEBREW_PREFIX" ]; then
  export DOTNET_ROOT="$HOMEBREW_PREFIX/opt/dotnet/libexec"
  export PATH="$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools"
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
  export DOCKER_HOST="ssh://colima"
fi

# vpns
# vpn
alias vpn='sudo openvpn ~/vpns/"$(ls ~/vpns | fzf)"'

