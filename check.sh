#!/bin/bash

set -ex

elm-make --yes

pushd examples
elm-make --yes TaskExample.elm
elm-test --yes || true
popd
