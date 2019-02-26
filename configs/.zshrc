export TERM=xterm-256color

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="kardan"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy.mm.dd"

plugins=(git cp web-search colored-man)

export PATH="/usr/local/bin:$PATH"

source $ZSH/oh-my-zsh.sh

alias ls='ls --classify --color=auto --human-readable --time-style=locale --group-directories-first'
alias nautilus='nautilus --no-desktop'
alias df='df --human-readable'
alias du='du --human-readable'
alias grep='grep --color'
alias cal='ncal -Mb'
alias ag='ag --hidden'

# psql horizontal scroll
export PAGER=cat
export LESS="-iMSx4 -XFR"

# keyboard settings
setxkbmap -option "grp_led:scroll,ctrl:nocaps,grp:caps_toggle,grp:shifts_toggle" \
    -layout "us,ru"

# IPONWEB
# CVS_RSH=ssh
# CVSROOT=:ext:mnikolaev@timesheets.iponweb.net:/var/cvs; export CVSROOT
export CVS_RSH=ssh; export CVSROOT=:ext:mnikolaev@timesheets.iponweb.net:/var/cvs


# Docker aliases
alias dockerps="docker ps --format='table {{.ID}}\t{{.Status}}\t{{.Names}}'"

dockerlogs() {
    name="$1"
    docker logs "${@:2}" $(docker ps --format='{{.Names}}' | grep "$name")
}

dockerexec() {
    name="$1"
    echo $ENV_MODE
    docker exec -it $(docker ps --format='{{.Names}}' | grep "$name") "${@:2}"
}
