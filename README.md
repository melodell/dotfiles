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

## Package management
Manually-maintained lists of Homebrew and Python packages are in `.homebrew.txt` and `.python.txt`.

Update package lists.

```
brew list > .homebrew.txt
pip freeze > .python.txt
```

NOTE: GUI applications (like Emacs and Docker) are installed with `brew install --cask`.
`~/bin/install` keeps a list of cask applications.

Install.

```console
$ ./bin/install
+ brew install --cask emacs
...
+ cat .homebrew.txt | xargs brew install
...
+ cat .python.txt | xargs pip install
```

