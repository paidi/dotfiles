while read pkg; do
    pipx install $pkg
done < packages/pipx.txt

# Install recent python versions
pyenv install 3.9.2
pyenv install 3.8.6
pyenv install 3.7.10
