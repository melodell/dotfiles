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
brew list > homebrew.txt
```

Install.

```
cat homebrew.txt | xargs brew install
```

