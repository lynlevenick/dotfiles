#!/usr/bin/env false

if test -r "${HOME}/.rvm/scripts/rvm"; then
	if test -t 1; then
		alias bundle='unalias bundle gem irb ruby rvm && . "${HOME}/.rvm/scripts/rvm" && bundle'
		alias gem='unalias bundle gem irb ruby rvm && . "${HOME}/.rvm/scripts/rvm" && gem'
		alias irb='unalias bundle gem irb ruby rvm && . "${HOME}/.rvm/scripts/rvm" && irb'
		alias ruby='unalias bundle gem irb ruby rvm && . "${HOME}/.rvm/scripts/rvm" && ruby'
		alias rvm='unalias bundle gem irb ruby rvm && . "${HOME}/.rvm/scripts/rvm" && rvm'
	else
		. "${HOME}/.rvm/scripts/rvm"
	fi

	__pathadd "${HOME}/.rvm/bin"
fi
