# nback

[![CircleCI](https://circleci.com/gh/mujx/nback.svg?style=svg)](https://circleci.com/gh/mujx/nback)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Implementation of the n-back game with a terminal UI.

### Installation

Install [stack](https://docs.haskellstack.org/en/stable/README/) and run the following

```bash
stack install
```

### Controls

- <kbd>SPACE</kbd> - Start a session.
- <kbd>A</kbd> - Audio match.
- <kbd>L</kbd> - Visual match.
- <kbd>C</kbd> - Cancel the running trial and drop back to the main menu.
- <kbd>U</kbd> - Increase the difficulty level by one.
- <kbd>D</kbd> - Decrease the difficulty level by one.
- <kbd>G</kbd> - Generate a chart with the progress.
- <kbd>ESC</kbd> / <kbd>Q</kbd>  - Exit the application.

### Usage

```
nback :: v0.1.0

Usage: nback [-f|--stat-file FILE] [-l|--level LEVEL] [-t|--trials TRIALS]
  The N-Back game

Available options:
  -f,--stat-file FILE      File to save trial's
                           summary (default: "/home/user/.local/share/nback/trials.log")
  -l,--level LEVEL         Specify the N-Back level to start (default: 2)
  -t,--trials TRIALS       The number of trials in a session (default: 20)
  -h,--help                Show this help text
```
