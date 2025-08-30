#!/bin/bash

# Legacy Component Import Finder
# Usage: ./find_legacy_imports.sh <directory>

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'
DIM='\033[2m'

# Check if directory argument is provided
if [ $# -eq 0 ]; then
    echo -e "${RED}Error: Please provide a directory to search${NC}"
    echo "Usage: $0 <directory>"
    exit 1
fi

SEARCH_DIR="$1"

# Check if directory exists
if [ ! -d "$SEARCH_DIR" ]; then
    echo -e "${RED}Error: Directory '$SEARCH_DIR' does not exist${NC}"
    exit 1
fi

echo -e "${BOLD}${BLUE}Legacy Component Import Finder${NC}"
echo -e "${DIM}Searching in: $SEARCH_DIR${NC}"
echo ""

# Function to search and format results with proper line numbers
search_and_display() {
    local title="$1"
    local color="$2"
    shift 2
    local patterns=("$@")
    
    local results=""
    local count=0
    
    for pattern in "${patterns[@]}"; do
        local current_file=""
        
        # Get all matches for this pattern
        while IFS= read -r line; do
            if [[ $line == *":"* ]]; then
                # Parse filepath:linenum:content
                local filepath=$(echo "$line" | cut -d: -f1)
                local linenum=$(echo "$line" | cut -d: -f2)
                local content=$(echo "$line" | cut -d: -f3-)
                
                # Print filepath header if it changed
                if [[ "$filepath" != "$current_file" ]]; then
                    if [[ -n "$current_file" ]]; then
                        results="$results\n"  # Add spacing between files
                    fi
                    results="${results}${color}${filepath}${NC}\n"
                    current_file="$filepath"
                fi
                
                # Check if this line contains the actual import pattern
                if echo "$content" | grep -qE "$pattern"; then
                    results="${results}${GREEN}${linenum}${NC}:${content}\n"
                    ((count++))
                fi
            elif [[ -z "$line" ]]; then
                # Skip empty lines
                continue
            fi
        done < <(grep -rn --include="*.ts" --include="*.tsx" --include="*.js" --include="*.jsx" \
                -E "$pattern" "$SEARCH_DIR" 2>/dev/null)
    done
    
    if [[ $count -gt 0 ]]; then
        echo -e "${BOLD}${color}$title ($count matches)${NC}" >&2
        echo -e "${DIM}$(printf '─%.0s' {1..50})${NC}" >&2
        echo -e "$results" >&2
    fi
    
    echo $count
}

echo -e "${YELLOW}Searching for legacy imports...${NC}"
echo ""

# Search for each category and capture counts
OLD_VERITY_TYPOGRAPHY_COUNT=$(search_and_display "Old Verity Typography Imports" "$PURPLE" \
    "import.*from.*['\"].*components-legacy/verity.*Typography" \
    "import.*from.*['\"].*verity/molecules.*Typography" \
    "import.*from.*['\"].*verity/atoms.*Typography" \
    "import.*from.*['\"].*verity/organisms.*Typography")

OLD_VERITY_BUTTON_COUNT=$(search_and_display "Old Verity Button Imports" "$RED" \
    "import.*from.*['\"].*verity/molecules/buttons" \
    "import[[:space:]]+Button.*from.*['\"].*verity/consumables/Button['\"]")

OLD_VERITY_LINK_COUNT=$(search_and_display "Old Verity Link Imports" "$CYAN" \
    "import[[:space:]]+Link.*from.*['\"].*verity/consumables/Link['\"]")

OLD_VERITY_ICON_COUNT=$(search_and_display "Old Verity Icon Imports" "$YELLOW" \
    "import.*from.*['\"].*verity/molecules/icons" \
    "import.*from.*['\"].*verity/consumables/Icon['\"]")

OLD_VERITY_CHECKBOX_COUNT=$(search_and_display "Old Verity Checkbox Imports" "$GREEN" \
    "import[[:space:]]+Checkbox.*from.*['\"].*verity/consumables/Checkbox['\"]")

COMMON_TYPOGRAPHY_COUNT=$(search_and_display "Common Typography Imports" "$PURPLE" \
    "import.*from.*['\"].*common/components.*localizedtext")

COMMON_BUTTON_COUNT=$(search_and_display "Common Button Imports" "$RED" \
    "import.*from.*['\"].*common/components.*button" \
    "import.*Button.*from.*['\"].*common/components")

COMMAND_BUTTON_COUNT=$(search_and_display "Command Button Imports" "$RED" \
    "import.*Button.*from.*['\"].*command/components" \
    "import.*Pill.*from.*['\"].*command/components" \
    "import.*chip.*from.*['\"].*command/components")

# Calculate grand total
GRAND_TOTAL=$((OLD_VERITY_TYPOGRAPHY_COUNT + OLD_VERITY_BUTTON_COUNT + OLD_VERITY_LINK_COUNT + OLD_VERITY_ICON_COUNT + COMMON_TYPOGRAPHY_COUNT + COMMON_BUTTON_COUNT + COMMAND_BUTTON_COUNT))

echo -e "${BOLD}${WHITE}SUMMARY${NC}"
echo -e "${DIM}$(printf '═%.0s' {1..50})${NC}"
echo -e "${PURPLE}Old Verity Typography:${NC} $OLD_VERITY_TYPOGRAPHY_COUNT"
echo -e "${RED}Old Verity Button:${NC} $OLD_VERITY_BUTTON_COUNT"
echo -e "${CYAN}Old Verity Link:${NC} $OLD_VERITY_LINK_COUNT"
echo -e "${GREEN}Old Verity Checkbox:${NC} $OLD_VERITY_CHECKBOX_COUNT"
echo -e "${YELLOW}Old Verity Icon:${NC} $OLD_VERITY_ICON_COUNT"
echo -e "${PURPLE}Common Typography:${NC} $COMMON_TYPOGRAPHY_COUNT"
echo -e "${RED}Common Button:${NC} $COMMON_BUTTON_COUNT"
echo -e "${RED}Command Button:${NC} $COMMAND_BUTTON_COUNT"
echo -e "${DIM}$(printf '─%.0s' {1..30})${NC}"
echo -e "${BOLD}${RED}Total Legacy Imports: $GRAND_TOTAL${NC}"

if [ $GRAND_TOTAL -eq 0 ]; then
    echo ""
    echo -e "${BOLD}${GREEN}🎉 No legacy imports found! Migration appears complete.${NC}"
else
    echo ""
    echo -e "${YELLOW}⚠️  Found $GRAND_TOTAL legacy imports that need migration.${NC}"
fi
