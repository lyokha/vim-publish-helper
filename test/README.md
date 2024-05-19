### Render example.html

```ShellSession
$ pandoc -thtml -Fvimhl example.md > example.html
```

### Render example.pdf

```ShellSession
$ bash vimhl_latex_tmpl.sh -s FFFF99 -f FF6699 -d > ~/.pandoc/templates/vimhl.latex
```

```ShellSession
$ pandoc -tpdf -Fvimhl --template=vimhl example.md > example.pdf
```

