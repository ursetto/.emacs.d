# My emacs config

## Requirements

- Emacs 26 or higher (only tested lately on >= 27.1)
- About 300MB for `straight.el` repositories + elpy virtualenv

## New installs

Nearly everything should be installed and configured automatically just by
starting emacs after cloning this repo. "Restricted" packages will be skipped
when behind a proxy.

Ensure Python 3 is installed for elpy, and run `M-x elpy-config` to check if
all elpy packages are installed. If not:

    M-x elpy-rpc-reinstall-virtualenv   ;; say yes to installing dependencies
    pipx install flake8
