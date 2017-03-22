# Adjrn

Adjrn is a terminal viewer of [jrnl](http://maebert.github.io/jrnl/)
files.

## Install

- Install `stack` from https://haskellstack.org or from your package
  manager.
- Clone this repo: `git clone https://github.com/timds/adjrn.git`
- Run `stack install` inside the adjrn directory to install an
  executable to `~/.local/bin`. You may move this executable to
  another location.

## Usage

`adjrn [journal_name]` views journal_name as listed in your jrnlconfig
file. If a name is not provided, attempts to view the journal named
'default'.

`adjrn filepath` views a regular, plain text journal file.

`adjrn filepath [--decrypt | -d]` views an encrypted journal file.

## To do

* Searching and tag filtering
* Non-default date formats
