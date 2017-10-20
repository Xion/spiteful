# spiteful

Mischievous Reddit bot

[![Build Status](https://img.shields.io/travis/Xion/spiteful.svg)](https://travis-ci.org/Xion/spiteful)
[![License](https://img.shields.io/github/license/Xion/spiteful.svg)](https://github.com/Xion/spiteful/blob/master/LICENSE)

## What it does?

Currently, it:

* looks for new posts that have "don't upvote" (or similar) in the title
  and _specifically upvotes them_

## Development notes

To install the ICU library on Windows (necessary for text-icu dependency),
use Stack's msys & Pacman:

    stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-icu
