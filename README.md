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
*g:html\_use\_css = 0* thus embedding color tags inside the generated HTML code.
Wraps the generated HTML code inside *&lt;pre&gt;&lt;tt&gt;* and
*&lt;/tt&gt;&lt;/pre&gt;* tags. Copy it in a clipboard and then insert in any
HTML document: an article in your blog, HTML book etc. The code highlights will
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
filter feature available from pandoc version 1.12.

### Basic usage

This distribution is shipped with a haskell program vimhl.hs which is supposed
to be such a filter. Normally one may want to compile it

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
article, find tags *&lt;pre&gt;* starting the codes and add there the attribute
*class="cpp"*

```html
<pre class="cpp">
```

After that you run pandoc to create TeX code from the original HTML article

```sh
pandoc -f html -t latex -o article.tex article.html
```

As far as pandoc finds attribute class="cpp" inside tags
*&lt;pre&gt; ... &lt;/pre&gt;* it generates its own code highlights based on the
editor Kate's engine. Now you can add another attribute *hl="vim"* inside tags
*&lt;pre&gt;*

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
*&lt;pre&gt;* to turn code enumeration on

```html
<pre class="cpp numberLines" hl="vim" startFrom="100">
```

### Using with dedicated .vimrc file

Running vim with normal $HOME/.vimrc and all the scripts in the directory
$HOME/.vim/ consumes many resources and unnecessarily slows pandoc down. To
fight this you can create a new file *.vimrc.pandoc* in your home directory with
very minimal settings. When vimhl.hs finds this file it runs vim with options
*--noplugin -u $HOME/.vimrc.pandoc*. As soon as plugins are turned off
.vimrc.pandoc must source at least plugins publish\_helper and TOhtml (for
producing HTML documents). Here is an example of good .vimrc.pandoc contents:

```vim
syntax on

filetype on
filetype indent on
filetype plugin on
filetype plugin indent on

let g:lucius_style = 'light'
let g:lucius_contrast = 'high'
let g:lucius_contrast_bg = 'high'

set nocp    " for line breaks with backslashes
colorscheme lucius

runtime plugin/tohtml.vim
runtime plugin/publish_helper.vim
```

You may need to source other plugins, for example TagHighlight which makes
possible to highlight tags generated by ctags.

### Options to choose color scheme

Here is the algorithm of choosing color scheme in priority order:

* If tag *&lt;pre&gt;* contains attribute *colorscheme="&lt;value&gt;"* then
  *&lt;value&gt;* is chosen, else

* If file $HOME/.vimrc.pandoc contains line *colorscheme &lt;value&gt;* then
  *&lt;value&gt;* is chosen, else

* If file $HOME/.vimrc contains line *let g:PhColorscheme = "&lt;value&gt;"*
  then *&lt;value&gt;* is chosen, else

* If file $HOME/.vimrc contains line *colorscheme &lt;value&gt;* then
  *&lt;value&gt;* is chosen, else

* System vim color scheme is chosen

The second case, i.e. when colorscheme is defined in file $HOME/.vimrc.pandoc,
is preferable as vim will consume less resources and work fastest.

### Remarks

* Be aware that when running vimhl.hs may create temporary files
  *\_vimhl\_buffer* and *\_vimhl\_result* that will be deleted after finishing
  the job.

* Tag class may contain a list of values. To make vimhl.hs work properly the
  file type must be the first value in the list.

* Normally pandoc adds definitions of Shaded and Highlighting environments in
  TeX output when it finds CodeBlock branches in generated AST. Publish helper
  will replace CodeBlock branches with RawBlock branches and pandoc may skip
  inserting those definitions. In this case you can add it manually in the
  preamble of the TeX document:

  ```tex
  \usepackage{color}
  \usepackage{xcolor}
  \usepackage{fancyvrb}
  \newcommand{\VerbBar}{|}
  \newcommand{\VERB}{\Verb[commandchars=\\\{\}]}
  \DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\{\}}
  \usepackage{framed}
  \newenvironment{Shaded}{
    \definecolor{shadecolor}{rgb}{1.0, 1.0, 0.9}
    \setlength\parskip{0cm}
    \setlength\partopsep{-\topsep}
    \addtolength\partopsep{0.2cm}
    \begin{shaded}
      \scriptsize
  }{\end{shaded}}
  ```

  All settings inside environment Shaded are optional. For example value of
  shadecolor defines background color of the code block: if you do not want
  that code blocks in your documents have specific background color then just
  do not define it in Shaded environment.

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

