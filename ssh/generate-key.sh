#! /usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail

main() {
    local script_dir=$(pwd)/$(dirname "$0")

    local username
    printf 'Username: ' ; read username

    local host
    printf 'Host: ' ; read host

    local format=ed25519
    local output="${script_dir}/id_${format}_${username}@${host}"
    ssh-keygen -t "${format}" -f "${output}"

    local answer
    printf 'Add to agent? (Y/n): ' ; read answer
    if [ "${answer}" != 'n' ] && [ "${answer}" != 'N' ] ; then
        ssh-add -K "${output}"
    fi
}

main "$@"
