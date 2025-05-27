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

export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

CFLAGS="-I$(brew --prefix xz)/include" LDFLAGS="-L$(brew --prefix xz)/lib" pyenv install 3.9.6
CFLAGS="-I$(brew --prefix xz)/include" LDFLAGS="-L$(brew --prefix xz)/lib" pyenv install 3.8.11
CFLAGS="-I$(brew --prefix xz)/include" LDFLAGS="-L$(brew --prefix xz)/lib" pyenv install 3.7.11
