#!/bin/sh
#

sudo apt-get install emacs
sudo apt-get install rxvt-unicode
sudo apt-get install xmonad
sudo apt-get install ipython
sudo apt-get install conkeror
sudo apt-get install mercurial
sudo apt-get install git
sudo apt-get install ttf-dejavu ttf-inconsolata
sudo apt-get install python-dulwich
sudo apt-get install python-subversion
sudo apt-get install curl
sudo apt-get install irssi irssi-plugin-xmpp
sudo apt-get install python-pip
sudo pip install mercurial_keyring


mkdir -p ~/lib/
hg clone http://bitbucket.org/durin42/dotfiles ~/lib/durin42-dotfiles
hg clone http://bitbucket.org/durin42/nosemacs ~/lib/nosemacs
hg clone http://bitbucket.org/durin42/hgsubversion ~/lib/hgsubversion
hg clone http://bitbucket.org/durin42/hg-git ~/lib/hggit
hg clone http://js2-mode.googlecode.com/svn/trunk/ ~/lib/js2-mode
hg clone http://hg.intevation.org/mercurial/crew ~/lib/mercurial-crew
hg clone http://bitbucket.org/durin42/histedit/ ~/lib/histedit
hg clone git://github.com/defunkt/coffee-mode.git ~/lib/coffee-mode
hg clone https://bitbucket.org/sjl/mercurial-cli-templates ~/lib/mercurial-cli-templates

mkdir -p ~/.xmonad/
mkdir -p ~/.ssh/
mkdir -p ~/.irssi/

ln -s $PWD/conkerorrc ~/.conkerorrc
ln -s $PWD/.emacs ~/.emacs
ln -s $PWD/.emacs-extras ~/.emacs-extras
ln -s $PWD/gnomerc ~/.gnomerc
ln -s $PWD/hgrc ~/.hgrc
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/Xmodmap ~/.Xmodmap
ln -s $PWD/Xresources ~/.Xresources
ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/ipythonrc ~/.ipythonrc
ln -s $PWD/ssh-config ~/.ssh/config
ln -s $PWD/irssi-config ~/.irssi/config


cat > ~/.bashrc <<EOF
pushd ~/etc/shlib > /dev/null

. .profile

popd > /dev/null

export PATH=\$PATH:/var/lib/gems/1.8/bin
export PATH=\$PATH:~/bin
EOF
