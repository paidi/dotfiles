#!/bin/bash

# install extra pyenv plugins
pyenv_plugins=(
    jawshooah/pyenv-default-packages
)

for plugin in "${pyenv_plugins[@]}"; do
    plugin_name=$(echo $plugin | cut -d '/' -f2)
    git clone https://github.com/$plugin $(pyenv root)/plugins/$plugin_name
done

while read pkg; do
    pipx install $pkg
done < packages/pipx.txt

# Install recent python versions
pyenv install 3.9.2
pyenv install 3.8.6
pyenv install 3.7.10
