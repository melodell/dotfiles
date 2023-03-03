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

## Install Homebrew packages

```
cat homebrew.txt | xargs brew install
```

