# spiteful

Mischievous Reddit bot

[![Build Status](https://img.shields.io/travis/Xion/spiteful.svg)](https://travis-ci.org/Xion/spiteful)
[![License](https://img.shields.io/github/license/Xion/spiteful.svg)](https://github.com/Xion/spiteful/blob/master/LICENSE)

## What it does?

Currently, it:

* looks for new posts that have "don't upvote" (or similar) in the title
  and _specifically upvotes them_
* looks for new comments that contain "does anyone else?" (or similar)
  and replies with _No._
* checks for upvote pleas (posts with "pls upvote" or similar)
  and downvotes them
* listens for upvote baits (posts with "if this gets X upvotes" or similar)
  and downvotes them as well!

All those features can be turned on or off via the `--features` flag.

## Development notes

To install the ICU library on Windows (necessary for text-icu dependency),
use Stack's msys & Pacman:

    stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-icu

On OS X:

    brew install icu4c
    # run the first build
    stack build \
      --extra-include-dirs=/usr/local/opt/icu4c/include \
      --extra-lib-dirs=/usr/local/opt/icu4c/lib

On most Linux distros, it should Just Work (tm).
