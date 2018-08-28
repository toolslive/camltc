#!/bin/bash

set -e
set -u

git submodule init
git submodule update

VERSION_FILE=_build/default/src/camltc_version.ml
## Create a tarball from the current checkout.
NAME=$(git describe --tags)
if ! git describe --tags --exact-match 2> /dev/null; then
    echo "Cowardly refusing to create a release that has not been tagged"
    exit 1
fi
echo "Creating release for tag: ${NAME}"

rm -f ${VERSION_FILE}
dune build

# Checkout a shallow copy, including submodules.
URL=$(git remote get-url origin)
rm -rf ${NAME}
git clone ${URL} --single-branch --branch ${NAME} --depth=1 --shallow-submodules --recurse-submodules ${NAME}
cp ${VERSION_FILE} ${NAME}/src
tar czf ${NAME}.tar.gz --exclude-vcs --exclude-vcs-ignores ${NAME}
rm -rf ${NAME}
