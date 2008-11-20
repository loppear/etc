# Postgres
PATH=/opt/local/lib/postgresql83/bin:$PATH
PGDATA=/opt/local/var/db/postgresql83/defaultdb

# Mysql
PATH=/opt/local/lib/mysql5/bin:$PATH

# Ports
PATH=/opt/local/bin:/opt/local/sbin:$PATH
MANPATH=/opt/local/share/man:$MANPATH

# Me
PATH=~/bin:/usr/local/bin:$PATH
PYTHONPATH=/usr/local/lib/svn-python:/usr/local/lib/python2.5/site-packages

alias ls="ls -Gh"
alias sz="eazysvn"
alias pg_ctl="sudo -u postgres pg_ctl -D $PGDATA"
alias post-review="post-review.py --target-groups=CSMP -o"

svnurlof () {
       local url
       url=`svn info $1 2> /dev/null | grep '^URL: ' | sed 's/URL: //'`
       if [ x"$url" = "x" ]
       then
               url=`git svn info $1 2> /dev/null | grep '^URL: ' | sed 's/URL: //'`
       fi
       local root
       root=$(cd $1 ; hg root 2> /dev/null)
       if [ x$root != 'x' ]
       then
               if [ -e "$root/.hg/svn/url" ]
               then
                       url=`cat "$root/.hg/svn/url"`
                       local branch
                       branch=$(cd $1 ; hg branch)
                       if [ "$branch" != "default" ]
                       then
                               url=$url/branches/$branch
                       else
                               url=$url/trunk
                       fi
               fi
       fi
       if [ x"$url" = "x" ]
       then
               echo -n 'No repo found (tried svn, git-svn, hgsvnclient)'
       fi
       echo $url
}

alias hgserve='hg serve -p 7777 --style=gitweb --pid-file ~/.hgpid -d'
alias hgcleanup='hg st| grep "[orig|rej]$" | sed "s/\? \(.*\)/\1/"'

export PS1="[\u@\h:\w]\n$ "

export PATH
export PYTHONPATH
export MANPATH
export LIBPATH
export PGDATA
