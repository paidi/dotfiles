#!/bin/bash
set -e

curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash

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

export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

pyenv install 3.9.6
pyenv install 3.8.11
pyenv install 3.7.11
