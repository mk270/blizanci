#!/bin/bash

# No magic. This script is just a wrapper because the necessary commands
# aren't trivially discoverable.

# TODO install prerequisites
# TODO build

set -eu
cd $(dirname $0)

setup-env () {
    # Line things up such that cron and cgi-bin play nicely
    export HOME=~
    export PATH=/bin:/usr/bin
    export SHELL=/bin/bash
}

usage () {
    local name=$(basename $0)
    echo "Usage: $name [ start-daemon | stop-daemon | repl ]"
    exit 0
}

if [ $# -ne 1 ]; then
    usage
fi

APP=blizanci
REL=_build/default/rel/${APP}/bin/${APP}

case "$1" in
     start-daemon)
         setup-env
         $REL start
         ;;
     stop-daemon)
         setup-env
         $REL stop
         ;;
     repl)
         rebar3 shell
         ;;
     *)
         usage
         ;;
esac

