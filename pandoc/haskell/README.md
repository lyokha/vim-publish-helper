Pandoc filter *vimhl*. Build and install with commands

```ShellSession
$ cabal build
$ cabal install
```

The program is also available at *Hackage*, so you can install it with

```ShellSession
$ cabal install pandoc-vimhl
```

In runtime, use environment variable `VIMHL_BACKEND` to choose between vim
flavors. For example

```ShellSession
$ export VIMHL_BACKEND=nvim
```

or

```ShellSession
$ export VIMHL_BACKEND=/usr/local/bin/vim
```

