#!/usr/bin/env false

if test -r "${HOME}/.bashrc"; then
    . "${HOME}/.bashrc"
elif test -r "${HOME}/.profile"; then
    . "${HOME}/.profile"
fi
