# Simple theme based on my old zsh settings.

function get_host {
	echo '@'`hostname`''
}

function hg_dirty() {
    [[ $( hg status 2> /dev/null ) != "!" ]] &&	{
       echo $ZSH_THEME_GIT_PROMPT_DIRTY; }
}

function hg_prompt_info() {
  branch=`hg identify -b 2>/dev/null`
  hg root >/dev/null 2>/dev/null && echo "($branch$(hg_dirty))" && return
}

PROMPT='> '
RPROMPT='%~$(git_prompt_info)$(hg_prompt_info)'

ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"