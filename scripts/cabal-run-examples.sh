#!/bin/bash

RED='\033[31m'
GREEN='\033[32m'
RESET='\033[0m'

error_files=()
error_outputs=()

# Skip files with "action" in their name
for file in $(find examples -name "*.l4" | grep -v "action"); do
    output=$(cabal run lam4-cli -- "$file" 2>&1)
    exit_status=$?
    echo "$output"
    if [ $exit_status -ne 0 ]; then
        echo -e "${RED}Error running lam4-cli on $file${RESET}"
        error_files+=("$file")
        error_outputs+=("$output")
    fi
done

if [ ${#error_files[@]} -ne 0 ]; then
    echo;
    echo -e "${RED}==========================================================${RESET}"
    echo -e "${RED}  Non-actions-using programs with errors${RESET}"
    echo -e "${RED}==========================================================${RESET}"
    echo;

    for i in "${!error_files[@]}"; do
        echo -e "${RED}lam4-cli errored on ${error_files[$i]}${RESET}"
        echo -e "${RED}${error_outputs[$i]}${RESET}"
    done
else
    echo -e "${GREEN}All .l4 files without 'action' in filename in examples dir ran successfully.${RESET}"
fi