#!/usr/bin/env bash
# Shared helpers sourced by the other setup scripts.

# Run a command with sudo, unless already root (e.g. inside a Docker build).
maybe_sudo() {
    if [ "$(id -u)" -eq 0 ]; then
        "$@"
    else
        sudo "$@"
    fi
}

# Read a package-list file, skipping blank lines and comments, and echo
# one entry per line (entries may contain spaces/flags, e.g. "foo --bar").
read_package_list() {
    local file="$1" line
    while IFS= read -r line || [ -n "$line" ]; do
        line="${line%%#*}"
        line="$(echo -n "$line" | xargs)" # trim whitespace
        [ -n "$line" ] && echo "$line"
    done < "$file"
}
