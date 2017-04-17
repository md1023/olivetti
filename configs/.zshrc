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

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git cp web-search colored-man)

# User configuration

# export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# m-nikolaev customizations:
alias ls='ls --classify --color=auto --human-readable --time-style=locale --group-directories-first'
alias l1='ls -1'
alias nautilus='nautilus --no-desktop'
alias em='emacsclient -c -a ""&'
alias df='df --human-readable'
alias du='du --human-readable'
alias grep='grep --color'
alias cal='ncal -Mb'
alias ag='ag --hidden'

# psql horizontal scroll
# export PAGER=cat
# export LESS="-iMSx4 -XFR"

# virtualenvwrapper is installed locally with:
# pip install --install-option="--user" virtualenvwrapper
source $HOME/.local/bin/virtualenvwrapper.sh

# connect to svn:// behind proxy
# sudo apt-get install libnet-proxy-perl
# connect-tunnel -v -L -P 192.168.200.105:8088 -T 10234:79.104.197.230:9036
# svn checkout svn://localhost:10234 .
# svn update --username nikolaev --password nikolaev

# git clone https://github.com/thejoshwolfe/svn-color ~/Documents/svn-color
alias svn="python -u $HOME/Documents/svn-color/svn-color.py"

# beep from speakers
_alarm() {
    ( speaker-test --frequency $1 --test sine )&
    pid=$!
    sleep 0.${2}s
    kill -9 $pid
}
alias alarm='(_alarm 440 2 > /dev/null)'
# "( speaker-test -t sine -f 440 > /dev/null )& pid=$! ; sleep 0.2s ; kill -9 $pid"

# kill trailing whitespace
alias kill_trailing_whitespace='sed -i -r "s/\s+$//g" `hg st -man | xargs`'
# put two newlines before class definition
class_double_newlines() {
    for f in $@; do
        cat $f | \
            tr "\n" "\r" | \
            sed "s/\r\+class/\r\r\rclass/g" | \
            tr "\r" "\n" > $f".copy"
        mv $f".copy" $f
    done
}

# keyboard settings
setxkbmap -option "grp_led:scroll,ctrl:nocaps,grp:caps_toggle,grp:shifts_toggle" \
    -layout "us,ru"
