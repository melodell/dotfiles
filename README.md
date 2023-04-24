# dotfiles

## Install

```
git clone git@github.com:melodell/dotfiles.git
rsync -av dotfiles/ ~/
rm -rf dotfiles/
```

## Add new files

```
git add -f <filename>
```

## Homebrew package management
Update package list.

```
brew list > .homebrew.txt
```

NOTE: GUI applications (like Emacs and Docker) are installed with `brew install --cask`.
`~/bin/brewinstall` keeps a list of cask applications.

Install.

```console
$ ./bin/brewinstall
+ brew install --cask emacs
...
+ cat .homebrew.txt | xargs brew install
```

