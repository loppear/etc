#!/bin/sh
#

sudo apt-get install emacs24 ipython  mercurial git magit js2-mode python-mode ttf-dejavu fonts-inconsolata curl inotify-tools tmux

# nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.2/install.sh | bash

mkdir -p ~/lib/
hg clone http://bitbucket.org/durin42/dotfiles ~/lib/durin42-dotfiles
hg clone http://bitbucket.org/durin42/nosemacs ~/lib/nosemacs
git clone https://github.com/magnars/expand-region.el.git ~/lib/expand-region.el
git clone https://github.com/muennich/urxvt-perls.git ~/lib/urxvt-perls

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

sudo ln -s ~/lib/urxvt-perls/clipboard /usr/lib/urxvt/perl/
sudo ln -s ~/lib/urxvt-perls/url-select /usr/lib/urxvt/perl/
sudo ln -s $PWD/xmonad.session /usr/share/gnome-session/sessions/xmonad.session
sudo ln -s $PWD/xmonad-unity-session.desktop /usr/share/xsessions/xmonad-unity-session.desktop

cat > ~/.bashrc <<EOF
pushd ~/etc/shlib > /dev/null

. .profile

popd > /dev/null

export PATH=\$PATH:~/.gem/ruby/2.1.0/bin
export PATH=\$PATH:~/bin
EOF
