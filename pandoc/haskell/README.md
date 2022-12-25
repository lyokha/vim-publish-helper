Pandoc filter *vimhl*. Configure and build with commands

```ShellSession
$ cabal configure
$ cabal build
```

Install with command

```ShellSession
$ cabal install
```

or globally (if applicable), being a superuser

```ShellSession
# cabal install --global
```

Use environment variable `VIMHL_BACKEND` to choose between vim flavors. For
example

```ShellSession
$ export VIMHL_BACKEND=nvim
```

or

```ShellSession
$ export VIMHL_BACKEND=/usr/local/bin/vim
```

