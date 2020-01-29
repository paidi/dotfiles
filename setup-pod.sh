# Script to set things up on fresh k8s pod
apt-get install -y --no-install-recommends emacs git

curl -L git.io/antigen > antigen.zsh

sed -i 's/usr\/local\/share\/antigen/dotfiles/' .zshrc
