#!/bin/bash

which -s brew
if [[ $? != 0 ]] ; then
    if [[ "$OSTYPE" == linux* ]]; then
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
	eval $($HOME/.linuxbrew/bin/brew shellenv)
    else
	softwareupdate --install xcode-select
	xcode-select --install
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    fi
else
    brew update
fi

# Install homebrew deps
while read pkg; do
    brew install $pkg
done < packages/homebrew.txt

echo 'export PATH="/opt/homebrew/opt/llvm@11/bin:$PATH"' >> ~/.zshrc
echo 'OPENBLAS="$(brew --prefix openblas)"' >> user/.zshrc
