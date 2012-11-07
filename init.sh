#!/bin/sh
#

sudo add-apt-repository ppa:cassou/emacs
sudo add-apt-repository ppa:chris-lea/node.js
sudo apt-get update

sudo apt-get install emacs24
sudo apt-get install rxvt-unicode xsel
sudo apt-get install xmonad
sudo apt-get install ipython
sudo apt-get install conkeror
sudo apt-get install mercurial
sudo apt-get install git
sudo apt-get install magit
sudo apt-get install ttf-dejavu ttf-inconsolata
sudo apt-get install python-dulwich
sudo apt-get install python-subversion
sudo apt-get install curl
sudo apt-get install irssi irssi-plugin-xmpp
sudo apt-get install python-pip
sudo pip install mercurial_keyring


sudo apt-get install nodejs npm
sudo npm install -g coffeelint coffee-script




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
git clone https://github.com/magnars/expand-region.el.git ~/lib/expand-region.el
git clone https://github.com/muennich/urxvt-perls.git ~/lib/urxvt-perls

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
ln -s $PWD/irssi-startup ~/.irssi/startup

sudo ln -s ~/lib/urxvt-perls/clipboard /usr/lib/urxvt/perl/
sudo ln -s ~/lib/urxvt-perls/url-select /usr/lib/urxvt/perl/
sudo ln -s $PWD/xmonad.session /usr/share/gnome-session/sessions/xmonad.session
sudo ln -s $PWD/xmonad-unity-session.desktop /usr/share/xsessions/xmonad-unity-session.desktop

cat > ~/.bashrc <<EOF
pushd ~/etc/shlib > /dev/null

. .profile

popd > /dev/null

export PATH=\$PATH:/var/lib/gems/1.8/bin
export PATH=\$PATH:~/bin
EOF
