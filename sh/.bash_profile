#!/usr/bin/env false

if [ -r "${HOME}/.bashrc" ]; then
    . "${HOME}/.bashrc"
elif [ -r "${HOME}/.profile" ]; then
    . "${HOME}/.profile"
fi
