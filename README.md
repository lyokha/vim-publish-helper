Vim-publish-helper
==================

About
-----

Vim plugin publish\_helper provides two basic commands MakeHtmlCodeHighlight
and MakeTexCodeHighlight to produce HTML or TeX code respectively from the
contents of the current buffer or a part of the current buffer (in Visual
mode). The generated code opens up in a new window and contains color tags
that cite colors from the original buffer according to the current vim color
scheme or a color scheme declared by variable g:PhColorscheme.

The distribution of the plugin is shipped with a haskell program vimhl.hs that
may act as a filter for a great format conversion tool pandoc enabling it to
make use of vim's internal syntax highlighting engine when converting formats
to HTML or TeX.

Basic commands
--------------

### MakeHtmlCodeHighlight

Produces HTML code from the contents of the current buffer or a selected part
of it. Uses plugin TOhtml internally with temporarily set variable
g:html\_use\_css = 0 thus embedding color tags inside the generated HTML code.
Wraps the generated HTML code inside &lt;pre&gt;&lt;tt&gt; and
&lt;/tt&gt;&lt;/pre&gt; tags. Copy it in a clipboard and then insert it in any
HTML document: an article in your blog, HTML bookk etc. The code highlights will
look the same as in your vim session!

### MakeTexCodeHighlight

Basically this command is a twin of the previous one, only it produces a TeX
code that is compatible with pandoc generated TeX documents. As such the
generated TeX code contains color tags corresponding to the vim color scheme
used and is wrapped inside tags

```tex
\begin{Shaded}
\begin{Highlighting}[]
```

and

```tex
\end{Highlighting}
\end{Shaded}
```

The command may accept an optional argument that defines how the generated
code will be enumerated in the resulting document. If this argument is missing
then no enumeration will be generated. Otherwise if it is a positive integer
then the number of the first line of the generated code will be equal to its
value, if it is a negative integer then the number of the first line will be
equal to the number of the first line in the original buffer.

vimhl.hs and pandoc
-------------------
This is probably the most interesting feature of the plugin. Both commands
MakeHtmlCodeHighlight and MakeTexCodeHighlight can be used as drivers to the
vim syntax highlighting engine from pandoc. This is achieved via pandoc's
filter feature available from pandoc version 1.12. This distribution is
shipped with a haskell program vimhl.hs which is supposed to be such a filter.
Normally one may want to compile it

```sh
ghc --make vimh
```

and move produced binary executable file vimhl in some directory listed in the
environment variable $PATH. Alterantively one may make original file vimhl.hs
executable

```sh
chmod +x vimhl.hs
```

and move it somewhere in the $PATH as well. The first method is preferable.
After that pandoc gets capable to produce HTML or TeX code with authentic vim
syntax highlights! Let's make an example. Say you want to convert an HTML
article from your cool IT blog with multiple examples of C++ codes into PDF
format via pandoc HTML-to-TeX conversion engine. Normally you open the
article, find tags &lt;pre&gt; starting the codes and add there the attribute
class="cpp"

```html
<pre class="cpp">
```

After that you run pandoc to create TeX code from the original HTML article

```sh
pandoc -f html -t latex -o article.tex article.html
```

As far as pandoc finds attribute class="cpp" inside tags
&lt;pre&gt; ... &lt;/pre&gt; it generates its own code highlights based on the
editor Kate's engine. Now you can add another attribute hl="vim" inside tags
&lt;pre&gt;

```html
<pre class="cpp" hl="vim">
```

and run pandoc with the filter vimhl (or vimhl.hs if you did not compile
vimhl)

```sh
pandoc -f html -t latex -F vimhl -o article.tex article.html
```

If you then generate the PDF document from the article.tex the codes will be
highlighted exactly as they were highlighted inside vim! As soon as command
MakeTexCodeHighlight accepts the optional argument which defines that
generated code must be enumerated you can put usual pandoc options inside tags
&lt;pre&gt; to turn code enumeration on

```html
<pre class="cpp numberLines" hl="vim" startFrom="100">
```

Miscellaneous commands
----------------------

There are two additional commands GetFgColorUnderCursor and
GetBgColorUnderCursor. They have nothing to do with the code highlighting task
and were added for debugging purposes only. You can map them like

```vim
nmap <silent> ,vc :GetFgColorUnderCursor<CR>
nmap <silent> ,vb :GetBgColorUnderCursor<CR>
```

and find foreground or background colors under cursor with a simple keystroke.

### GetFgColorUnderCursor

Get foreground color under cursor.

### GetBgColorUnderCursor

Get background color under cursor.

Configuration
-------------

You can specify dedicated color scheme for syntax highlights by
MakeHtmlCodeHighlight and MakeTexCodeHighlight in variable g:PhColorscheme.
For example

```vim
let g:PhColorscheme = 'lucius'
```

If the variable is not defined then the current color scheme will be used.

Thanks to
---------

Christian Brabandt for plugin Colorizer and Xterm2rgb translation functions.

Issues
------

* MakeTexCodeHighlight does not work properly with control characters in code.

