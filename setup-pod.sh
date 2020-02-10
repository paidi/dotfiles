# Script to set things up on fresh k8s pod
apt-get install -y --no-install-recommends zsh
pip install awscli

curl -L git.io/antigen > antigen.zsh

sed -i 's@/usr/local/share/antigen@'"$PWD"'@' .zshrc

./symlink-setup.sh
