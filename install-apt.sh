#!/bin/bash
# exit when any command fails
set -e

# Install apt deps
while read pkg; do
    apt-get install -y --no-install-recommends $pkg
done < packages/apt.txt

mkdir $HOME/bin
curl -L git.io/antigen > $HOME/bin/antigen.zsh
