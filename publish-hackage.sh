#!/bin/bash

set -e

dir=$(mktemp -d)
dir=$(mktemp -d)
trap 'rm -rf "$dir" "$dir"' EXIT


function heading {
  echo -e "\n\n$@\n--------------------------------------------------------------------------------\n"
}

heading "Prerequisites"
echo "Documentation distribution directory: $dir"
echo "Documentation build directory: $dir"

PUBLISH_FLAG=""
if [[ "$1" == "--publish" ]]; then
    PUBLISH_FLAG="--publish"
fi

echo
read -p "About to publish all Motor packages to Hackage. Are you sure? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
  
  heading "Building Distributions"
  cabal v2-sdist all --builddir="$dir"
  
  heading "Building Documentation"
  cabal v2-haddock all --builddir="$dir" --haddock-for-hackage --haddock-hyperlink-source

  heading "Uploading Packages"
  
  for file in $dir/sdist/*.tar.gz ; do
    echo "Uploading distribution: $file"
    cabal upload $PUBLISH_FLAG "$file"
  done
  
  for file in $dir/*-docs.tar.gz ; do
    echo "Uploading documentation: $file"
    cabal upload $PUBLISH_FLAG -d "$file"
  done

else
    echo "Aborting."
fi
