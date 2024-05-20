#!/bin/bash
#set -x

pro_dir="$(cd "$(dirname "$0")/.." && pwd)"
cd "$pro_dir/doom.d/lisp/" || exit

function dis_type_figure() {
    m="$(echo "$1" | tr 'x' ' ' | awk 'BEGIN{a=0}{b=$1*$2; if(b>a) {a=b}}END{print a}')"
    if [ "$m" -ge 8294400 ]; then
        echo "hidpi"
    elif [ "$m" -ge 2073600 ]; then
        echo "fhd"
    else
        echo "normal"
    fi
}

if [ -n "$1" ]; then
    dis_type="$1"
else
    resolution="$(xrandr -q | grep '\*' | awk '{print $1}')"
    echo -e "resolution info:\n$resolution"
    dis_type=$(dis_type_figure "$resolution")
    echo "display type: $dis_type"
fi

if ! [ -f "font-conf.el.$dis_type" ]; then
    echo "file font-conf.el.$dis_type not find"
    exit 1
fi

echo "link font-conf.el to font-conf.el.$dis_type"
ln -sf font-conf.el{."$dis_type",}
