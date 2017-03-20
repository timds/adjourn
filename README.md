# Adjrn

Adjrn is a terminal viewer of [jrnl](http://maebert.github.io/jrnl/) files.

## Install

- Install `stack` from https://haskellstack.org or your package manager.
- Clone this repo: `git clone https://github.com/timds/adjrn.git`
- Run `stack install` inside the adjrn directory to install to `~/.local/bin`. 
  You may move this executable to another location.

## Usage

`adjrn journal_name` views a journal with the specified name.

`adjrn filepath` views a regular, plain text journal file.

`adjrn filepath [--decrypt | -d]` views an encrypted journal file.

## To do

* Handle alternative date formats
* Searching and filtering
