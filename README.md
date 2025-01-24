# dotfiles

## Install

```console
$ git clone git@github.com:melodell/dotfiles.git
$ rsync -av dotfiles/ ~/
$ rm -rf dotfiles/
```

## Track new files

```console
$ git add -f <filename>
```

## Package management

Manually-maintained lists of Homebrew and Python packages are in `.brew.txt` and `.pip.txt`.

NOTE: GUI applications (like Emacs and Docker) are installed with `brew install --cask`.
`~/bin/install` keeps a list of cask applications.

Install.

```console
$ ./bin/install
```

Upgrade.

```console
$ ./bin/upgrade
```
