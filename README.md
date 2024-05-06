# dotfiles

## Install

```console
git clone git@github.com:melodell/dotfiles.git
rsync -av dotfiles/ ~/
rm -rf dotfiles/
```

## Add new files

```console
git add -f <filename>
```

## Package management

Manually-maintained lists of Homebrew and Python packages are in `.brew.txt` and `.pip.txt`.

Update package lists.

```console
brew list > .brew.txt
pip freeze > .pip.txt
```

NOTE: GUI applications (like Emacs and Docker) are installed with `brew install --cask`.
`~/bin/install` keeps a list of cask applications.

Install.

```console
$ ./bin/install
+ brew install --cask emacs
...
+ cat .brew.txt | xargs brew install
...
+ cat .pip.txt | xargs pip install
```

Upgrade.

```console
$ ./bin/upgrade
+ brew update
+ brew upgrade
...
+ cat .pip.txt | xargs pip install --upgrade
```
