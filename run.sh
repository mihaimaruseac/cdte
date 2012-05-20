#!/bin/bash

EXEC="cdte"
RUN="./"${EXEC}" +RTS -N2 --RTS"

die () {
    echo "Cannot build."
    exit 1
}

[ -x ${EXEC} ] || make
[ -x ${EXEC} ] || die

if [ $# -ge 1 ]; then
    ${RUN} $1
else
    ${RUN}
fi
