# .bash_profile
#
# Melina O'Dell


# Homebrew configuration for x86_64 and arm64 MacOS
if [ -e /opt/homebrew ]; then
  export HOMEBREW_PREFIX=/opt/homebrew
elif [ -e /usr/local/Homebrew ]; then
  export HOMEBREW_PREFIX=/usr/local
fi
if [ -e ${HOMEBREW_PREFIX}/bin/brew ]; then
  export HOMEBREW_NO_AUTO_UPDATE=1
  eval $(${HOMEBREW_PREFIX}/bin/brew shellenv)
fi


### PATHS ###
# Curl
export PATH="/opt/homebrew/opt/curl/bin:$PATH"
# Java
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"
# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
# gnu-sed
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
# pipx
export PATH="$PATH:$HOME/.local/bin"
# proto
export PROTO_HOME="$HOME/.proto";
export PATH="$PROTO_HOME/shims:$PROTO_HOME/bin:$PATH";
# inv
export PATH="$PATH:$HOME/Library/Python/3.9/bin"


### ALIASES ###
### ls
LS=ls
if which gls &> /dev/null; then
  # Use GNU ls (coreutils) on MacOS
  LS='gls'
fi
LSCOLORS=' --color=auto --human-readable'
alias ls='${LS} ${LSCOLORS}'
alias la='${LS} -a ${LSCOLORS}'
alias ll='${LS} -l ${LSCOLORS}'
alias sl=ls  # I will not be getting steam-locomotive'd

### grep (colors)
alias grep='grep --color=auto'

### less
### Stolen from awdeorio
export PAGER="less --chop-long-lines"
alias less="${PAGER}"

### tar
# Extract
function tarx { echo "+ tar -xzvf $1"; tar -xzvf $1; }

### Git
alias gs='git status'
alias gd='git diff'
alias gr='git rebase'
alias gf='git fetch -p'
alias gfp='git fetch -p && git pull'
alias gb='git branch'
alias gco='git checkout'

# V: Prepend ticket/branch number to commit message
function gc {
    NAME=$(git branch | grep '^\*' | cut -b3- | cut -d "-" -f1 -f2)
    git commit -m "$NAME: $1"
}

# V: Create and push to new remote tracking branch
function gpo {
    git push -u origin $(git branch | grep '^\*' | cut -b3-)
}

# V: Refresh master
function grf {
    BRANCH=$(git branch | grep '^\*' | cut -b3-)
    git stash
    git switch master
    gf
    git pull
    git switch "$BRANCH"
    git stash pop
}

### ssh
# CAEN Linux
alias caen='ssh melodell@login.engin.umich.edu'

# Sync dotfiles to CAEN
function dotsync()
{
    rsync -rtv .bash_profile .bash_colors melodell@login.engin.umich.edu:
    rsync -rtv .emacs.d/init.el melodell@login.engin.umich.edu:.emacs.d
}

### Emacs
function e { emacs "$@" & }
function enw { emacs -nw -Q "$@"; }
function ediff { emacs --eval "(ediff-files \"$1\" \"$2\")" & }

### Python
# Start live HTTP server on port 8000
alias phs='python3 -m http.server 8000'
# Activate virtual env
function a { source "$@"/bin/activate; }

### Brave
alias brave='open -a "Brave Browser.app" -n --args --new-window'
alias brave-private='open -a "Brave Browser.app" -n --args --incognito'

### PostgreSQL ($ brew install postgresql)
# Start and stop default database cluster directory
alias pgstart='pg_ctl -D /opt/homebrew/var/postgresql@14/ start'
alias pgstop='pg_ctl -D /opt/homebrew/var/postgresql@14/ stop'

### Navigation
# TODO org directory
function cdtodo { cd "${HOME}/org/todo/"; }

# Shortcut to eecs485staff projects
function cd485()
{
    PROJECT_NUM="$1"
    case $PROJECT_NUM in
        p1|1)
            PROJECT='p1-insta485-static'
            ;;
        p2|2)
            PROJECT='p2-insta485-serverside'
            ;;
        p3|3)
            PROJECT='p3-insta485-clientside'
            ;;
        p4|4)
            PROJECT='p4-mapreduce'
            ;;
        p5|5)
            PROJECT='p5-search-engine'
            ;;
        madoop)
            PROJECT='madoop'
            ;;
        ag)
            PROJECT='ag-docker-images'
            ;;
        site)
            PROJECT='eecs485.org'
            ;;
        *)
            PROJECT=''
            ;;
    esac
    cd ${HOME}/src/eecs485staff/$PROJECT
}


### PROMPT ###
set -o emacs                              # Emacs CL mode
export HISTCONTROL="ignoredups"           # Ignore dup cmds
export BASH_SILENCE_DEPRECATION_WARNING=1 # Suppress default shell warning
export SUDO_EDITOR="emacs -Q -nw"         # Editor used by sudoedit and sudo -e
export GIT_EDITOR="emacs -Q -nw"          # Editor used by git commit
shopt -s checkwinsize                     # keep LINES and COLUMNS up to date

# Git context
# Based on https://github.com/awdeorio/dotfiles/blob/main/.bashrc
# ...which is based on https://github.com/jimeh/git-aware-prompt
function set_git_context() {
  # Branch
  local BRANCH
  local GIT_BRANCH
  if BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
      if [[ "$BRANCH" == "HEAD" ]]; then
          BRANCH="detached*"
      fi
      GIT_BRANCH="$BRANCH"
  else
      GIT_BRANCH=""
  fi

  # '*' and red for dirty; yellow for behind; green for clean
  # Note: This fails when a branch name has capital letters that match git's dirty letters
  local GIT_DIRTY
  local COLOR=${txtgrn}
  local STATUS=$(git status -sb 2> /dev/null);
  if [[ $(echo "$STATUS" | grep "M ") != "" || $(echo "$STATUS" | grep "? ") != "" || $(echo "$STATUS" | grep "D ") != "" || $(echo "$STATUS" | grep "A ") != "" ]]; then
      GIT_DIRTY='*'
      COLOR=${txtred}
  else
      GIT_DIRTY=''
  fi
  if [[ $(echo "$STATUS" 2> /dev/null | grep behind) != "" ]]; then
	  COLOR=${txtylw}
  fi

  # Concatenate
  local GIT_CONTEXT="${GIT_BRANCH}${GIT_DIRTY}"
  if [[ "$GIT_CONTEXT" != "" ]]; then
      GIT_PROMPT="$COLOR(${GIT_CONTEXT}) "
  else
      GIT_PROMPT=""
  fi
}

# Venv context
# Based on https://gist.github.com/insin/1425703/f22c4231a7b28b8f420d79158b5229e5ebd3fcd9
function set_venv_context() {
    # Only display if venv is active
    if $(test -z "$VIRTUAL_ENV"); then
        VENV_PROMPT=""
    else
        local VIRTUAL_ENV_BASE=$(basename "$VIRTUAL_ENV")
        VENV_PROMPT="${txtcyn}(${VIRTUAL_ENV_BASE}) "
    fi
}

# Set full bash prompt
# Mainly inspired by https://github.com/awdeorio/dotfiles/blob/main/.bashrc
function set_prompt() {
    set_git_context
    set_venv_context

	# If terminal supports colors
	case "$TERM" in
		xterm*|rxvt*|Eterm*|eterm*|screen*)
			PS1='$VENV_PROMPT$GIT_PROMPT\[${bldcyn}\]\u@\h \[${bldblu}\]\W\n\[${bldblu}\]\$ \[${txtrst}\]'
			;;
		*)
			PS1="$ "
			;;
	esac
    export PS1
}

# Colors!
source ~/.bash_colors

# Execute function before displaying prompt
# But don't break the prompt by replacing previous behavior
# https://apple.stackexchange.com/questions/128998/how-to-open-a-new-terminal-tab-in-current-working-directory
PROMPT_COMMAND="set_prompt; $PROMPT_COMMAND"


### UTILS ###
# Git autocompletion
[ -f /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash ] \
    && . /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash

# Beta yarn start
alias ys="yarn start-dev-beta"

# Added by `rbenv init` on Thu Feb 27 11:35:55 PST 2025
eval "$(rbenv init - --no-rehash bash)"
