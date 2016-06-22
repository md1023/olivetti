export TERM=xterm-256color

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="kardan"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git cp web-search colored-man)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
export PATH=/usr/lib/lightdm/lightdm:$PATH

# m-nikolaev customizations:
alias ls='ls --classify --color=auto --human-readable --time-style=locale --group-directories-first'
alias nautilus='nautilus --no-desktop'
alias em='emacsclient -c -a ""&'
alias df='df --human-readable'
alias du='du --human-readable'
alias grep='grep --color'
alias cal='ncal -Mb'

# find python, javascript, xml files
alias findpy='find . \( -name "*test*" -o -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.py" -print0 | xargs -0e grep -n --color'
alias findpytest='find . \( -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.py" -print0 | xargs -0e grep -n --color'
alias findjs='find . \( -name "*test*" -o -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.js" -print0 | xargs -0e grep -n --color'
alias findjstest='find . \( -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.js" -print0 | xargs -0e grep -n --color'
alias findlisp='find . -name "*.el" -print0 | xargs -0e grep -n --color'
alias findcss='find . \( -name "*test*" -o -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.css" -print0 | xargs -0e grep -n --color'
alias findxml='find . \( -name "*test*" -o -path "*towel*" -o -path "*.git*" -prune \) -o \( -name "*.xsl" -o -name "*.xml" \) -print0 | xargs -0e grep -n --color'
alias findsql='find . -name "*.sql" -print0 | xargs -0e grep -n --color'
alias findpybtwn='find . \( -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.py" -print0 | xargs -0e btwngrep'
alias findjsbtwn='find . \( -path "*towel*" -o -path "*.git*" -prune \) -o -name "*.js" -print0 | xargs -0e btwngrep'
alias ag='ag --hidden'

# myscripts
export MAGRATHEA=$HOME/Work/magrathea
export WORKON_HOME=$HOME/work/virtualenvs
export FLASH=$HOME/Public/Flash_SVN/
export LEIZEN=$HOME/Work/leizen
export LEIZEN_LIBS=$HOME/Work/leizen/.env/lib/python2.7/site-packages

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
setxkbmap -option "grp_led:scroll,ctrl:nocaps,grp:caps_toggle,grp:ctrl_shift_toggle" \
    -layout "us,ru"

