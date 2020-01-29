#!/bin/bash


# to maintain cask ....
#     brew update && brew upgrade brew-cask && brew cleanup && brew cask cleanup`


# Install native apps

brew install caskroom/cask/brew-cask
# brew tap caskroom/versions

# daily
brew cask install dropbox

# dev
brew cask install iterm2
brew cask install sublime-text
brew cask install imagealpha
brew cask install imageoptim

# less often
brew cask install disk-inventory-x
brew cask install screenflow
brew cask install vlc
brew cask install gpgtools

brew cask install spotify

brew tap caskroom/fonts
brew cask install font-fira-code

brew cask install virtualbox

# Not on cask but I want regardless.

# File Multi Tool 5
# Phosphor
