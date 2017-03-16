# Adjrn

Adjrn is a terminal viewer of [jrnl](http://maebert.github.io/jrnl/) files.

## Install

- Install `stack` from https://haskellstack.org or your package manager.
- Clone this repo: `git clone https://github.com/timds/adjourn.git`
- Run `stack install` inside the adjrn directory to install to `~/.local/bin`. 
  You may move this executable to another location.

## Usage

`adjrn filepath` views a regular, plain text journal.

`adjrn filepath [--decrypt | -d]` views an encrypted journal.

## To do

* Allow specifying journal name only
* Handle alternative date formats
* Searching and filtering
