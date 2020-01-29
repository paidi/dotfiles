# Script to set things up on fresh k8s pod
curl -L git.io/antigen > antigen.zsh

sed -i 's/usr\/local\/share\/antigen/dotfiles/' .zshrc
