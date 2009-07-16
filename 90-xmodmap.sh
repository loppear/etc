#!/bin/sh
#
# Re-enable keyboard settings
# for /etc/pm/sleep.d/

case "$1" in
        hibernate|suspend)
                # Stopping is not required.
                ;;
        thaw|resume)
                if [ -e /home/luke/etc/Xmodmap ] ; then
                        /usr/bin/xmodmap /home/luke/etc/Xmodmap
                fi
                ;;
        *) exit $NA
                ;;
esac
