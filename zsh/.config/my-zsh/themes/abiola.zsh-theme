#!/usr/bin/env zsh

# ------------------------------------------------------------------------------
#
# Pure - A minimal and beautiful theme for oh-my-zsh
#
# Based on the custom Zsh-prompt of the same name by Sindre Sorhus. A huge
# thanks goes out to him for designing the fantastic Pure prompt in the first
# place! I'd also like to thank Julien Nicoulaud for his "nicoulaj" theme from
# which I've borrowed both some ideas and some actual code. You can find out
# more about both of these fantastic two people here:
#
# Sindre Sorhus
#   Github:   https://github.com/sindresorhus
#   Twitter:  https://twitter.com/sindresorhus
#
# Julien Nicoulaud
#   Github:   https://github.com/nicoulaj
#   Twitter:  https://twitter.com/nicoulaj
#
# ------------------------------------------------------------------------------

# Set required options
#
setopt prompt_subst

# Load required modules
#
autoload -Uz vcs_info

# Set vcs_info parameters
#
zstyle ':vcs_info:*' enable hg bzr git
zstyle ':vcs_info:*:*' unstagedstr '!'
zstyle ':vcs_info:*:*' stagedstr '+'
zstyle ':vcs_info:*:*' formats "$FX[bold]%r$FX[no-bold]/%S" "%b" "%%u%c"
zstyle ':vcs_info:*:*' actionformats "$FX[bold]%r$FX[no-bold]/%S" "%b" "%u%c (%a)"
# zstyle ':vcs_info:*:*' nvcsformats "%c" "" ""
zstyle ':vcs_info:*:*' nvcsformats "%~" "" ""

# Fastest possible way to check if repo is dirty
#
git_dirty() {
    # Check if we're in a git repo
    command git rev-parse --is-inside-work-tree &>/dev/null || return
    # Check if it's dirty
    command git diff --quiet --ignore-submodules HEAD &>/dev/null
    [ $? -eq 1 ] && echo "*"
}

# Display information about the current repository
#
repo_information() {
    git_info="%F{$(color)}${vcs_info_msg_0_%%/.} %F{$(color)}$vcs_info_msg_1_$(git_dirty) $vcs_info_msg_3_%f"
    info=$(echo $git_info)
    if [ "$vcs_info_msg_1_" = "" ]; then
        echo "%~"
    else
        echo "${info}%~"
    fi
}

# Displays the exec time of the last command if set threshold was exceeded
#
cmd_exec_time() {
    local stop=$(date +%s)
    local start=${cmd_timestamp:-$stop}
    let local elapsed=$stop-$start
    [ $elapsed -gt 5 ] && echo ${elapsed}s
}

# Get the intial timestamp for cmd_exec_time
#
preexec() {
    cmd_timestamp=$(date +%s)
}

print_host() {
    # [ "$(hostname)" != "localhost" ] && echo "[$(hostname)] "
}

print_nix() {
    [ -n "$IN_NIX_SHELL" ] && echo "[nix-shell] "
}

# Output additional information about paths, repos and exec time
#
precmd() {
    vcs_info # Get version control info before we start outputting stuff
    print -P "\n$(print_host)$(print_nix)$(repo_information) %F{yellow}$(cmd_exec_time)%f"
}

# gnome builder terminal hack
color() {
    if [ -z "$INSIDE_EMACS" ]; then
        echo "white"
    else
        echo "white"
    fi
}

# Define prompts
#
PROMPT="%(?.%F{$(color)}.%F{red})$%f " # Display a red prompt char on failure
#PROMPT="`cprp`❯ " # Display a red prompt char on failure
RPROMPT="%F{$(color)}${SSH_TTY:+%n@%m}%f" # Display username if connected via SSH

# ------------------------------------------------------------------------------
#
# List of vcs_info format strings:
#
# %b => current branch
# %a => current action (rebase/merge)
# %s => current version control system
# %r => name of the root directory of the repository
# %S => current path relative to the repository root directory
# %m => in case of Git, show information about stashes
# %u => show unstaged changes in the repository
# %c => show staged changes in the repository
#
# List of prompt format strings:
#
# prompt:
# %F => color dict
# %f => reset color
# %~ => current path
# %* => time
# %n => username
# %m => shortname host
# %(?..) => prompt conditional - %(condition.true.false)
#
# ------------------------------------------------------------------------------
