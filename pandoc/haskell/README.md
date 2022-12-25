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

Use environment variable `VIM_EXECUTABLE` to choose between vim flavors. For
example

```ShellSession
$ export VIM_EXECUTABLE=nvim
```

or

```ShellSession
$ export VIM_EXECUTABLE=/usr/local/bin/vim
```

