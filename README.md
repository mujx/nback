# nback

[![CircleCI](https://circleci.com/gh/mujx/nback.svg?style=svg)](https://circleci.com/gh/mujx/nback)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Implementation of the n-back game with a terminal UI.

### Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/) and run the following

```bash
stack install
```

### Usage

```
$ nback --help

nback :: v0.1.0

Usage: nback [-f|--stat-file FILE] [-l|--level LEVEL]
  The N-Back game

Available options:
  -f,--stat-file FILE      File to save trial's
                           summary (default: "/home/user/.local/share/nback/trials.log")
  -l,--level LEVEL         Specify the N-Back level to start (default: 2)
  -h,--help                Show this help text
```
