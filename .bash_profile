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

# Sync dotfiles to CAEN
function dotsync()
{
    rsync -rtv .bash_profile .bash_colors melodell@login.engin.umich.edu:
    rsync -rtv .emacs.d/init.el melodell@login.engin.umich.edu:.emacs.d
}

# Caen-specific aliases
if HOSTNAME=$(hostname 2> /dev/null | grep caen); then
    ## EECS484
    # SQLPlus alias
    alias sq='rlwrap sqlplus'
    # Load modules
    module load eecs484
    module load mongodb    
fi

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

### Temp ###
# W23 src directory shortcuts
function cd484 { cd /Users/melinaodell/src/eecs484/; }
function cd486 { cd /Users/melinaodell/src/eecs486/; }

if HOSTNAME=$(hostname 2> /dev/null | grep caen); then
    function cd484 { cd /home/melodell/src/eecs484/; }
    function cd486 { cd /home/melodell/src/eecs486/; }
fi
## End Temp ###

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
      if [[ "$BRANCH" == "HEAD" ]]; then
          BRANCH="detached*"
      fi
      GIT_BRANCH="$BRANCH"
  else
      GIT_BRANCH=""
  fi

  # '*' for dirty
  local GIT_DIRTY
  local COLOR=${txtgrn}
  local STATUS=$(git status -sb 2> /dev/null);
  if [[ $(echo "$STATUS" | wc -l | grep 1) == "" ]]; then
      GIT_DIRTY='*'
      COLOR=${txtred}
      if [[ $(echo "$STATUS" 2> /dev/null | grep behind) ]]; then
          COLOR=${txtylw}
      fi
  else
      GIT_DIRTY=''
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
        VIRTUAL_ENV_BASE=$(basename "$VIRTUAL_ENV")
        VENV_PROMPT="${txtcyn}(${VIRTUAL_ENV_BASE}) "
    fi
}

# Set full bash prompt
# Mainly inspired by https://github.com/awdeorio/dotfiles/blob/main/.bashrc
function set_prompt() {
    set_git_context
    set_venv_context

    PS1='$VENV_PROMPT$GIT_PROMPT\[${bldcyn}\]\u@\h \[${bldblu}\]\W\n\$ \[${txtrst}\]'
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
