# .bash_profile
#
# Melina O'Dell


# Homebrew configuration for x86_64 and arm64
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


### ALIASES ###
### ls
# Use GNU ls (from coreutils)
alias ls='gls --color=auto --human-readable'
alias la='ls -a'
alias ll='ls -l'

### grep (colors)
alias grep='grep --color=auto'

### tar
# Extract
function tarx { echo "+ tar -xzvf $1"; tar -xzvf $1; }

### Git
alias gs='git status'
alias gd='git diff'
alias gr='git rebase'
alias gf='git fetch -p'
alias gb='git branch'

### ssh
# CAEN Linux
alias caen='ssh melodell@login.engin.umich.edu'

### Emacs
function e { emacs "$@" & }

### Python
# Start live HTTP server on port 8000
alias phs='python3 -m http.server 8000'
# Activate virtual env
function a { source "$@"/bin/activate; }

### Brave
alias brave='open -a "Brave Browser.app" -n --args --new-window'
alias brave-private='open -a "Brave Browser.app" -n --args --incognito'

### Navigation

### TEMP ###
# W23 src directory shortcuts
function cd484 { cd /Users/melinaodell/src/eecs484/; }
function cd486 { cd /Users/melinaodell/src/eecs486/; }

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
    cd /Users/melinaodell/src/eecs485staff/$PROJECT
}


### PROMPT ###
set -o emacs                              # Emacs CL mode
export HISTCONTROL="ignoredups"           # Ignore dup cmds
export BASH_SILENCE_DEPRECATION_WARNING=1 # Suppress default shell warning

# Git context
# Based on https://github.com/awdeorio/dotfiles/blob/main/.bashrc
# ...which is based on https://github.com/jimeh/git-aware-prompt
function set_git_context() {
  # Branch
  local BRANCH
  local GIT_BRANCH
  if BRANCH=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
      if [[ "$branch" == "HEAD" ]]; then
          BRANCH="detached*"
      fi
      GIT_BRANCH="$BRANCH"
  else
      GIT_BRANCH=""
  fi

  # '*' for dirty
  local STATUS=$(git status --porcelain 2> /dev/null);
  if [[ "$STATUS" != "" ]]; then
      GIT_DIRTY='*'
  else
      GIT_DIRTY=''
  fi

  # Concatenate
  local GIT_CONTEXT="${GIT_BRANCH}${GIT_DIRTY}"
  GIT_PROMPT="(${GIT_CONTEXT}) "
}

# Venv context
# Based on https://gist.github.com/insin/1425703/f22c4231a7b28b8f420d79158b5229e5ebd3fcd9
function set_venv_context() {
    # Only display if venv is active
    if test -z "$VIRTUAL_ENV" ; then
        VENV_PROMPT=""
    else
        VIRTUAL_ENV_BASE=`basename "$VIRTUAL_ENV"`
        VENV_PROMPT="($VIRTUAL_ENV_BASE) "
    fi
}

# Set full bash prompt
# Mainly inspired by https://github.com/awdeorio/dotfiles/blob/main/.bashrc
function set_prompt() {
    set_git_context
    set_venv_context

    # Special colors for behind
    BEHIND=$(git status -sb 2> /dev/null | grep behind);
    if [[ "$BEHIND" != "" ]]; then
        PS1='\[${txtcyn}\]$VENV_PROMPT\[${txtylw}\]$GIT_PROMPT\[${bldcyn}\]\u@\h \[${bldblu}\]\W \$ \[${txtrst}\]'
    fi
        
    # Special colors for dirty/clean
    if [[ "$GIT_DIRTY" != "" ]]; then
        PS1='\[${txtcyn}\]$VENV_PROMPT\[${txtred}\]$GIT_PROMPT\[${bldcyn}\]\u@\h \[${bldblu}\]\W \$ \[${txtrst}\]'
    else
        PS1='\[${txtcyn}\]$VENV_PROMPT\[${txtgrn}\]$GIT_PROMPT\[${bldcyn}\]\u@\h \[${bldblu}\]\W \$ \[${txtrst}\]'
    fi
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
