#!/bin/sh
#
ln -s $PWD/conkerorrc ~/.conkerorrc
ln -s $PWD/.emacs ~/.emacs
ln -s $PWD/.emacs-extras ~/.emacs-extras
ln -s $PWD/gnomerc ~/.gnomerc
ln -s $PWD/hgrc ~/.hgrc
ln -s $PWD/Xmodmap ~/.Xmodmap
ln -s $PWD/Xresources ~/.Xresources
ln -s $PWD/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $PWD/ipythonrc ~/.ipythonrc

cat > ~/.bashrc <<EOF
pushd ~/etc/shlib > /dev/null

. .profile

popd > /dev/null

export PATH=$PATH:/var/lib/gems/1.8/bin
export PATH=$PATH:~/bin
EOF
