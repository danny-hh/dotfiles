#!/bin/sh

if [ "$TERM" = linux ]; then
    export PS1="-> "
    export PATH="$HOME/.local/share/bin:$PATH"
    export LANG=en_US.UTF-8
    export CFLAGS="-O2 -pipe -march=native"
    export CXXFLAGS="$CFLAGS"
    export MAKEFLAGS="-j$(nproc)"
    export XDG_RUNTIME_DIR="/tmp/1000"
    mkdir -p "${XDG_RUNTIME_DIR}"
fi

make() { make CC=gcc ;}
wget() { wget --no-hsts ;}
wttr() { curl wttr.in/taguig?format=1 ;}
send() { curl -F "file=@$1" 'https://x0.at' | wl-copy ;}
pick() { grim -g "$(slurp -p)" -t ppm - |
convert - -format '%[pixel:p{0,0}]' txt:- ;}

# view full command outputs without having to
# resize the terminal with a scrollable pager.
qq()   { eval "$@" | less -R ;}

hh1()  { du -bhd 1 ;}
hh2()  { xbps-query -l | wc -l ;}

ds1()  { df -Th ;}
ds2()  { df -h | grep 'nvme' ;}
ds3()  { ls -l /dev/disk/by-uuid/ ;}
ds4()  { lsusb ;}

fcl()  { fc-list ;}
fcf()  { fc-list : family | sort | uniq ;}
fci()  { fc-match -v | grep -E 'hint|rgb|lcd|dpi' ;}
