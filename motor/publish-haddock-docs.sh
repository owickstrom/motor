#!/bin/bash

set -e

dir=$(mktemp -d)
trap 'rm -r "$dir"' EXIT

cabal new-haddock all --builddir="$dir" --haddock-for-hackage --haddock-hyperlink-source

for file in $dir/*-docs.tar.gz ; do
  echo "$file"
  cabal upload --publish -d "$file"
done
