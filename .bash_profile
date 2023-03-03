# .bash_profile
#
# Melina O'Dell


### Homebrew for ARM
eval "$(/opt/homebrew/bin/brew shellenv)"


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
function cd485 ()
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
# Suppress default shell warning
export BASH_SILENCE_DEPRECATION_WARNING=1


### UTILS ###
# Git autocompletion
[ -f /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash ] \
    && . /Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash
