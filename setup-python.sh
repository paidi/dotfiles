set -e
export PYENV_ROOT=~/.pyenv
git clone https://github.com/pyenv/pyenv.git $PYENV_ROOT
cp packages/pyenv.txt $PYENV_ROOT/default_packages

export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

pyenv install 3.9.2
pyenv install 3.8.6
pyenv install 3.7.10

git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
