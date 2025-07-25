#!/bin/bash
#
# Git branch cleanup script
# Builds on https://github.com/not-an-aardvark/git-delete-squashed
#
# Melina O'Dell
#
# Usage: ./git-branch-cleanup.sh [-f|-d]
# -f: ACTUALLY delete merged branches
# -d: Dry run (list which branches would be deleted)

# Put your base branch name here (main, master, develop, etc.)
TARGET_BRANCH=master

# Colors for output
bldgrn="$(tput setaf 2 2>/dev/null)$(tput bold 2>/dev/null || echo '\e[1;32m')"  # Bold green
txtred="$(tput setaf 1 2>/dev/null || echo '\e[0;31m')"  # Red
txtylw="$(tput setaf 3 2>/dev/null || echo '\e[0;33m')"  # Yellow
txtrst="$(tput sgr 0 2>/dev/null || echo '\e[0m')"  # Text Reset

show_usage() {
    echo "Usage: $0 [-f|-d]"
    echo "  -f  ACTUALLY delete merged branches"
    echo "  -d  Dry run (list which branches would be deleted)"
    echo "  No flag: Interactive mode"
    exit 1
}

interactive_mode() {
    local options=("Dry run (list which branches would be deleted)" "ACTUALLY delete merged branches" "Exit")
    local selected=0
    local num_options=${#options[@]}

    while true; do
        # Clear screen and show header
        clear
        echo "Git Branch Cleanup"
        echo "=================="
        echo "Use ↑/↓ arrow keys to navigate, Enter to select:"
        echo

        # Display options with highlighting
        for i in "${!options[@]}"; do
            if [ $i -eq $selected ]; then
                echo "${bldgrn}* ${options[$i]}${txtrst}"
            else
                echo "  ${options[$i]}"
            fi
        done

        # Read a single character
        read -rsn1 key

        # Handle special keys (arrow keys send escape sequences)
        if [[ $key == $'\x1b' ]]; then
            read -rsn2 key
            case $key in
                '[A') # Up arrow
                    ((selected--))
                    if [ $selected -lt 0 ]; then
                        selected=$((num_options - 1))
                    fi
                    ;;
                '[B') # Down arrow
                    ((selected++))
                    if [ $selected -ge $num_options ]; then
                        selected=0
                    fi
                    ;;
            esac
        elif [[ $key == "" ]]; then # Enter key
            case $selected in
                0)
                    clear
                    echo "Dry running..."
                    dry_run
                    break
                    ;;
                1)
                    clear
                    echo "Deleting merged branches..."
                    delete
                    break
                    ;;
                2)
                    clear
                    exit 0
                    ;;
            esac
        fi
    done
}

dry_run() {
    git checkout -q master && git for-each-ref refs/heads/ "--format=%(refname:short)" | while read branch; do
    mergeBase=$(git merge-base master $branch) && [[ $(git cherry master $(git commit-tree $(git rev-parse "$branch^{tree}") -p $mergeBase -m _)) == "-"* ]] && echo "${txtylw}$branch${txtrst} is merged into master and can be deleted";
    done
}

delete() {
    git checkout -q master && git for-each-ref refs/heads/ "--format=%(refname:short)" | while read branch; do
    mergeBase=$(git merge-base master $branch) && [[ $(git cherry master $(git commit-tree $(git rev-parse "$branch^{tree}") -p $mergeBase -m _)) == "-"* ]] && echo "${txtred}$(git branch -D $branch)${txtrst}";
    done
}

# Parse command line arguments
case "${1:-}" in
    -f)
        echo "Are you sure you want to delete merged branches? (y/n)"
        read -r confirm
        if [[ $confirm != "y" && $confirm != "Y" ]]; then
            echo "Aborting deletion."
            exit 0
        fi
        clear
        delete
        ;;
    -d)
        echo "Dry running..."
        dry_run
        ;;
    -h|--help)
        show_usage
        ;;
    "")
        interactive_mode
        ;;
    *)
        echo "Error: Invalid option '$1'"
        show_usage
        ;;
esac