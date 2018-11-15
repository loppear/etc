#!/bin/sh
#

sudo apt-get install xmonad rxvt-unicode emacs24 ipython  mercurial git magit js2-mode python-mode ttf-dejavu fonts-inconsolata curl inotify-tools tmux chromium

# nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
nvm install 8.9
nvm alias default 8.9

mkdir -p ~/lib/
hg clone http://bitbucket.org/durin42/dotfiles ~/lib/durin42-dotfiles
git clone https://github.com/magnars/expand-region.el.git ~/lib/expand-region.el

mkdir -p ~/.xmonad/
mkdir -p ~/.ssh/
mkdir -p ~/.irssi/
mkdir -p ~/.emacs.d

ln -s $PWD/.emacs ~/.emacs
ln -s $PWD/.emacs-extras ~/.emacs-extras
ln -s $PWD/elpa ~/.emacs.d/elpa
ln -s $PWD/gnomerc ~/.gnomerc
ln -s $PWD/hgrc ~/.hgrc
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/Xmodmap ~/.Xmodmap
ln -s $PWD/Xresources ~/.Xresources
ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/ipythonrc ~/.ipythonrc
ln -s $PWD/ssh-config ~/.ssh/config
ln -s $PWD/irssi-config ~/.irssi/config
ln -s $PWD/irssi-startup ~/.irssi/startup
ln -s $PWD/eslintrc ~/.eslintrc

sudo ln -s $PWD/xmonad.session /usr/share/gnome-session/sessions/xmonad.session
sudo ln -s $PWD/xmonad-unity-session.desktop /usr/share/xsessions/xmonad-unity-session.desktop

cat > ~/.bashrc <<EOF
pushd ~/etc/shlib > /dev/null

. .profile

popd > /dev/null

export PATH=\$PATH:~/.gem/ruby/2.1.0/bin
export PATH=\$PATH:~/bin
. .nvm/nvm.sh
EOF

source ~/.bashrc

npm install -g eslint babel-eslint eslint-plugin-polymer eslint-plugin-json eslint-plugin-html eslint-config-google
