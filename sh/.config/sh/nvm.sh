#!/usr/bin/env false

if test -r "${HOME}/.nvm/nvm.sh"; then
	NVM_DIR="${HOME}/.nvm"; export NVM_DIR

	if test -t 1; then
		alias node='unalias node npm nvm yarn && . "${NVM_DIR}/nvm.sh" && node'
		alias npm='unalias node npm nvm yarn && . "${NVM_DIR}/nvm.sh" && npm'
		alias nvm='unalias node npm nvm yarn && . "${NVM_DIR}/nvm.sh" && nvm'
		alias yarn='unalias node npm nvm yarn && . "${NVM_DIR}/nvm.sh" && yarn'
	else
		. "${NVM_DIR}/nvm.sh"
	fi
fi
