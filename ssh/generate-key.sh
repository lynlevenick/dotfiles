#! /usr/bin/env sh

set -o errexit
set -o nounset
set -o pipefail

main() {
    local script_dir=$(pwd)/$(dirname "$0")

    local identifier
    printf 'Identifier: ' ; read identifier

    local comment
    printf 'Comment?: ' ; read comment

    local format=ed25519
    local output="${script_dir}/id_${format}_${identifier}"

    if [ "${comment}" != '' ] ; then
        ssh-keygen -t "${format}" -f "${output}" -C "${comment}"
    else
        ssh-keygen -t "${format}" -f "${output}"
    fi

    local answer
    printf 'Add to agent? (Y/n): ' ; read answer
    if [ "${answer}" != 'n' ] && [ "${answer}" != 'N' ] ; then
        ssh-add -K "${output}"
    fi
}

main "$@"
