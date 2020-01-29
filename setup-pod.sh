# Script to set things up on fresh k8s pod
apt-get install -y --no-install-recommends zsh

curl -L git.io/antigen > antigen.zsh

sed -i 's/usr\/local\/share\/antigen/$HOME\/dotfiles/' .zshrc

./symlink-setup.sh
