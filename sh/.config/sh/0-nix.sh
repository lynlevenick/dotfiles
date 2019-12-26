#!/usr/bin/env false

if test -e "$HOME/.nix-profile/etc/profile.d/nix.sh"; then
	# shellcheck disable=SC1090
	. "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi
