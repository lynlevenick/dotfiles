#!/usr/bin/env false

if test -d "$HOME/.cargo"; then
	CARGO_HOME="$HOME/.cargo"; export CARGO_HOME
	__pathadd "$CARGO_HOME/bin"
fi
