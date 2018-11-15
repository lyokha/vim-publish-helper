Vim-publish-helper
==================

Table of contents
-----------------

- [About](#about)
- [Basic commands](#basic-commands)
    + [MakeHtmlCodeHighlight](#makehtmlcodehighlight)
    + [MakeTexCodeHighlight](#maketexcodehighlight)
- [vimhl.hs and pandoc](#vimhl.hs-and-pandoc)
    + [Basic usage](#basic-usage)
    + [Using with dedicated .vimrc file](#using-with-dedicated-.vimrc-file)
    + [Customizing vim settings](#customizing-vim-settings)
    + [Options to choose color scheme](#options-to-choose-color-scheme)
    + [Remarks](#remarks)
- [Miscellaneous commands](#miscellaneous-commands)
    + [GetFgColorUnderCursor](#getfgcolorundercursor)
    + [GetBgColorUnderCursor](#getbgcolorundercursor)
- [Configuration](#configuration)
    + [g:PhColorscheme](#gphcolorscheme)
    + [g:PhHtmlEngine](#gphhtmlengine)
    + [g:PhHtmlPreAttrs](#gphhtmlpreattrs)
    + [g:PhTexBlockStyle](#gphtexblockstyle)
    + [g:PhCtrlTrans](#gphctrltrans)
    + [g:PhTrimBlocks](#gphtrimblocks)
    + [g:PhRichTextElems](#gphrichtextelems)
    + [g:PhLinenrAsTblColumn](#gphlinenrastblcolumn)
- [Highlighting shells and REPLs](#highlighting-shells-and-repls)
- [An example](#an-example)
- [Thanks to](#thanks-to)

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
Wraps the generated HTML code inside *&lt;pre&gt; ... &lt;/pre&gt;* tags. Copy
it in a clipboard and then insert in any HTML document: an article in your blog,
HTML book etc. The code highlights will look the same as in your vim session!

The command may accept an optional argument that defines if the generated
code will be numbered in the resulting document and what number the first line
will be. If this argument is missing then line numbers will not be generated.
Otherwise if it is a positive integer then the number of the first line of the
generated code will be equal to this value, if it is a negative integer then
the number of the first line will be equal to the number of the first line in
the original buffer.

Starting from **version 0.6** MakeHtmlCodeHighlight uses same highlighting
engine as MakeTexCodeHighlight by default. To switch back to TOhtml engine set
variable *g:PhHtmlEngine = 'tohtml'*.

Output of TOhtml may differ from that of default highlighting engine: it
renders buffers in a very verbose way and may content folds, bold text etc.
whereas default engine normally ignores view details of the buffer.

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

Starting from **version 0.9** the environment name (*Shaded*) is no longer
constant and defined by variable g:PhTexBlockStyle. This allows applying
different styles for code blocks in TeX documents.

vimhl.hs and pandoc
-------------------

This is the most useful feature of the plugin. Both the commands
MakeHtmlCodeHighlight and MakeTexCodeHighlight can be used as drivers to the
vim syntax highlighting engine from pandoc. This is achieved via pandoc's
filter feature available from pandoc version 1.12.

### Basic usage

This distribution is shipped with a haskell program vimhl.hs which is supposed
to be such a filter. Normally one may want to compile it

```ShellSession
$ ghc --make vimhl
```

and move produced binary executable file vimhl in some directory listed in the
environment variable &#36;PATH. Alternatively vimhl can be installed with cabal

```ShellSession
$ cabal install pandoc-vimhl
```

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

```ShellSession
$ pandoc -f html -t latex -o article.tex article.html
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

```ShellSession
$ pandoc -f html -t latex -F vimhl -o article.tex article.html
```

If you then generate the PDF document from the article.tex the codes will be
highlighted exactly as they were highlighted inside vim! As soon as command
MakeTexCodeHighlight accepts the optional argument which defines that
generated code must be numbered you can put usual pandoc options inside tags
*&lt;pre&gt;* to turn code numbering on

```html
<pre class="cpp numberLines" hl="vim" startFrom="100">
```

### Using with dedicated .vimrc file

Running vim with normal &#36;HOME/.vimrc and all the scripts in the directory
&#36;HOME/.vim/ consumes many resources and unnecessarily slows pandoc down. To
fight this you can create a new file *.vimrc.pandoc* in your home directory with
very minimal settings. When vimhl.hs finds this file it runs vim with options
*--noplugin -u &#36;HOME/.vimrc.pandoc*. As soon as plugins are turned off
.vimrc.pandoc must source at least plugins publish\_helper and TOhtml (for
producing HTML documents, but since version 0.6 of this plugin this is
optional). Here is an example of good .vimrc.pandoc contents:

```vim
set nocompatible

filetype off    " filetype is set by vimhl

let g:lucius_style = 'light'
let g:lucius_contrast = 'high'
let g:lucius_contrast_bg = 'high'

colorscheme lucius
syntax on

let g:PhCtrlTrans = 0

runtime plugin/publish_helper.vim
```

You may need to source other plugins, for example TagHighlight which makes
possible to highlight tags generated by ctags.

### Customizing vim settings

Starting from **version 0.7** vimhl accepts a new attribute *vars* to define
global vim variables. The example of a custom .vimrc.pandoc script from the
previous section contains definition of a global variable g:PhCtrlTrans. Now you
can remove this definition from the script and set variable g:PhCtrlTrans
dynamically from the filter only for those code blocks that require it. To
accomplish this put the new attribute *vars="PhCtrlTrans"* in such code blocks.

Global variables are also good for making selection between arbitrary
conditions. Imagine that script .vimrc.pandoc has lines

```vim
if exists('g:load_TagHl')
    colorscheme bandit
    runtime plugin/TagHighlight.vim
    let g:TagHighlightSettings['LanguageDetectionMethods'] = ['FileType']
endif

if exists('g:PhHtmlEngine') && g:PhHtmlEngine == 'tohtml'
    runtime plugin/tohtml.vim
    let g:html_no_progress = 1
    let g:html_ignore_folding = 1
endif
```

The first condition says that if a global variable *g:load_TagHl* exists then
vimhl must use color scheme *bandit* and load plugin *TagHighlight* that would
normally add extra highlighting groups to make code highlights look rich and
more beautiful. The second condition says that if a global variable
*g:PhHtmlEngine* exists and is equal to *tohtml* then vimhl must load plugin
TOhtml.

The new attribute *vars* allows loading vim global variables from the original
document. To turn conditions in the example above on it must be defined as
*vars="load_TagHl,PhHtmlEngine=tohtml"*. This example shows that variables must
be delimited by *commas*, their values are defined after *equal sign*, if the
equal sign is missing then the value is supposed to be equal to *1*,
*quote signs* around the value and the prefix *g:* before the variable name are
missing and will be substituted transparently inside vimhl.

### Options to choose color scheme

Here is the algorithm of choosing color scheme in priority order:

* If tag *&lt;pre&gt;* contains attribute *colorscheme="&lt;value&gt;"* then
  *&lt;value&gt;* is chosen, else

* If file &#36;HOME/.vimrc.pandoc contains line *colorscheme &lt;value&gt;* then
  *&lt;value&gt;* is chosen, else

* If file &#36;HOME/.vimrc contains line *let g:PhColorscheme = "&lt;value&gt;"*
  then *&lt;value&gt;* is chosen, else

* If file &#36;HOME/.vimrc contains line *colorscheme &lt;value&gt;* then
  *&lt;value&gt;* is chosen, else

* System vim color scheme is chosen

The second case, i.e. when colorscheme is defined in file
&#36;HOME/.vimrc.pandoc, is preferable as vim will consume less resources and
work fastest.

### Remarks

* Tag class may contain a list of values. To make vimhl.hs work properly the
  file type must be the first value in the list.

* Normally pandoc adds definitions of Shaded and Highlighting environments in
  TeX output when it finds CodeBlock branches in generated AST. Publish helper
  will replace CodeBlock branches with RawBlock branches and pandoc may skip
  inserting those definitions. In this case you can add it manually in the
  preamble of the TeX document:

    ```tex
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

  You may also want to use script *vimhl_latex_tmpl.sh* shipped with the plugin
  in order to facilitate this task. The script prints to stdout a pandoc
  template for Latex which is compatible with vimhl. Besides *Shaded*
  environment it defines *Snugshade*, *Framed*, *Leftbar* and *Mdframed*
  environments that correspond to definitions of the same names in Latex
  packages Framed and Mdframed.

  Normally the output has to be redirected to a file in the standard pandoc
  templates directory.

    ```ShellSession
  $ sh vimhl_latex_tmpl.sh > ~/.pandoc/templates/vimhl.latex
    ```

  Now you can use this template for making standalone TeX or PDF documents
  using pandoc's option *--template=vimhl*. The script accepts a number of
  options to customize visual parameters of code blocks. To see them use
  option *-h*.

* If you have installed vim plugin for pandoc
  (http://www.vim.org/scripts/script.php?script_id=3730) then you'll probably
  notice bad syntax highlighting when editing .vimrc.pandoc from vim. To make
  it normal change content of .vim/ftdetect/pandoc.vim to

    ```vim
  au BufNewFile,BufRead *.markdown,*.md,*.mkd,*.pd,*.pdc,*.pdk,*.text
              \ set filetype=pandoc
  au BufNewFile,BufRead *.pandoc
              \ if expand('<afile>:t') != '.vimrc.pandoc' | set filetype=pandoc
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

### g:PhColorscheme

```vim
let g:PhColorscheme = 'lucius'
```

This variable specifies dedicated color scheme for syntax highlights by
MakeHtmlCodeHighlight and MakeTexCodeHighlight. If not set then the current
color scheme will be used. Do not set it in .vimrc.pandoc as normal setting of
color scheme is preferred there.

### g:PhHtmlEngine

```vim
let g:PhHtmlEngine = 'tohtml'
```

Available since **version 0.6**. If value is *tohtml* then TOhtml engine will be
used to render HTML highlights, otherwise the internal engine will be used.
Not set by default.

### g:PhHtmlPreAttrs

This variable sets attributes that will be inserted inside tags *&lt;pre&gt;* in
the generated HTML documents. Examples:

```vim
let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap;"'
```

```vim
let g:PhHtmlPreAttrs = 'style="overflow-x: auto;"'
```

### g:PhTexBlockStyle

This variable sets visual parameters of code blocks in the generated TeX
documents. If not set then *Shaded* environment is used. Examples:

```vim
let g:PhTexBlockStyle = 'Shaded'
```

```vim
let g:PhTexBlockStyle = 'Framed'
```

### g:PhCtrlTrans

```vim
let g:PhCtrlTrans = 1
```

Some programming languages allow using verbatim control characters. For
example you may define an interactive scenario in viml with command *normal*
which may require them. This variable specifies that MakeTexCodeHighlight will
accurately translate verbatim control characters in their usual vim ascii
representation. Setting this variable for using from vimhl.hs does not always
work as expected because some values (like ^M) may have been already lost on
the pandoc's AST level. This variable is not set by default.

### g:PhTrimBlocks

```vim
let g:PhTrimBlocks = 0
```

This variable defines if blank lines around code blocks will be removed. Set
to 1 by default.

### g:PhRichTextElems

```vim
let g:PhRichTextElems = ['bg', 'bold', 'italic']
```

This variable defines a list of rich text elements that will be accepted for
rendering text both in HTML and Latex formats, it is ignored when using TOhtml
engine. Accepted values are *bg*, *bold*, *italic* and *underline*, other
values are quietly ignored. All accepted elements are turned on by default.
Notice that Latex engine uses *\colorbox* for rendering background which
normally has outstanding height that makes the whole line higher. To prevent
this put

```tex
\setlength\fboxsep{1pt}
```

in the preamble of the TeX document. Script vimhl_latex_tmpl.sh puts this line
in environments Shaded, Framed and Mdframed automatically.

### g:PhLinenrAsTblColumn

```vim
let g:PhLinenrAsTblColumn = 1
```

Draw line-numbered code as an HTML table. Effective only when internal syntax
highlighting engine is used. Not set by default. There are a few variables to
control how various elements of the table will look.

* *g:PhLinenrColumnBorderAttrs* defines border attributes between the
  line-number and the code columns. Beware: it does not expect color settings,
  see the next clause. Default value is *1px solid*.

* *g:PhLinenrTblBottomPadding* defines padding on the bottom of the table.
  Default value is *0*.

* *g:PhLinenrFgColor* defines foreground color of the line-number column and of
  the border between the columns. Not set by default: color of the
  *SpecialKey* syntax highlighting group is used in this case.

* *g:PhLinenrColumnWidth* defines the line-number column width. Default value is
  *2em*.

* *g:PhLinenrColumnAttrs* defines attributes of the line-number column. Empty by
  default. May be used to customize background color of the column.

* *g:PhCodeColumnOverflowX* defines overflow-x behaviour of the code column.
  Default value is *auto*. This value must correspond to non-wrapping text
  models, otherwise line numbers may mismatch code lines if the latter wraps.

Highlighting shells and REPLs
-----------------------------

The option for highlighting various shells and REPLs (bash, ghci, python REPL
etc.) is available from **version 0.10** of the plugin. Normally one may want to
highlight shells and REPLs blocks in a different way than code blocks. This is
easily achieved by specifying a variable that defines a role of the block.
Imagine that we want to use filter vimhl in pandoc, then the role might be
defined via a variable passed in the attribute vars: *vars="PhBlockRole=output"*
and the block view would be customized in .vimrc.pandoc like this:

```vim
if !exists('g:PhHtmlPreAttrs')
    let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; background: #FFE"'
endif

if exists('g:PhBlockRole') && g:PhBlockRole == 'output'
    let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; '.
        \ 'display: inline-block; border-style: none none none solid; '.
        \ 'border-color: blue; border-width: 15px; padding: 5px 10px"'
    let g:PhTexBlockStyle = 'Leftbar'
endif
```

But what shall we do with the highlights? There is no syntax for universal
shell prompts and outputs. We must invent it! The plugin contains very simple
definition of such a syntax in file *syntax/shelloutput.vim*. It means that the
name for this "language" is *shelloutput* (you can change it via variable
*g:PhShellOutputFt* however it makes little sense without renaming the syntax
file). This filetype is magic for the plugin. It defines a *virtual prompt*:
value of the variable *g:PhShellOutputPrompt* ("||| ", i.e. *three-bars-space*
by default). Lines that start with the virtual prompt (including blank
characters before it) signal user input in the shell and are highlighted as
Statement syntax items, other lines are supposed to be the shell output. The
virtual prompt is ignored in resulting documents because it only plays a role of
a marker for making correct highlights in the document.

Highlighting shells and REPLs in TeX documents requires extra definitions in
the preamble because it utilizes language definition feature of the Latex
package Listings. Here is an example:

```tex
\usepackage{MnSymbol}
\usepackage{listings}
\definecolor{shellpromptcolor}{HTML}{000000}
\definecolor{shelloutputcolor}{HTML}{666666}
\lstset{basicstyle=\scriptsize\ttfamily, breaklines=true}
\lstset{prebreak=\raisebox{0ex}[0ex][0ex]
    {\ensuremath{\rhookswarrow}}}
\lstset{postbreak=\raisebox{0ex}[0ex][0ex]
    {\ensuremath{\rcurvearrowse\space}}}
\lstdefinelanguage{shelloutput}
    {basicstyle=\color{shelloutputcolor}
        \scriptsize
        \ttfamily\itshape,
     moredelim=[il][\color{shellpromptcolor}\upshape]{|||\ }}
```

Script vimhl_latex_tmpl.sh has several options to insert these definitions
automatically in a pandoc template. Using these definitions has advantage of
inserting pretty line breaks in the document automatically when they are needed.
If you want to set up different *also-letters* (for the notion of this see
documentation for the package Listings) in a shell block you can define them in
variable *g:PhAlsoletter*.

An example
----------

- Pandoc flavoured markdown source file example.md

    ```pandoc
        ### Original example from [*Pandoc User's Guide*](http://johnmacfarlane.net/pandoc/README.html#fenced-code-blocks)

        ``` {#mycode .haskell .numberLines hl="vim" startFrom="99"}
        qsort []     = []
        qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                       qsort (filter (>= x) xs)
        ```

        ### Content of my *~/.vimrc.pandoc*

        ``` {#vimrc_pandoc .vim .numberLines hl="vim" vars="PhTexBlockStyle=Mdframed"}
        set nocompatible

        filetype off    " filetype is set by vimhl

        let g:lucius_style = 'light'
        let g:lucius_contrast = 'high'
        let g:lucius_contrast_bg = 'high'

        colorscheme lucius
        syntax on

        if !exists('g:PhHtmlPreAttrs')
            let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; background: #FFE"'
        endif

        if exists('g:PhBlockRole') && g:PhBlockRole == 'output'
            let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; '.
                    \ 'display: inline-block; border-style: none none none solid; '.
                    \ 'border-color: blue; border-width: 15px; padding: 5px 10px"'
            let g:PhTexBlockStyle = 'Leftbar'
        endif

        runtime plugin/publish_helper.vim

        if exists('g:PhHtmlEngine') && g:PhHtmlEngine == 'tohtml'
            runtime plugin/tohtml.vim
            let g:html_no_progress = 1
            let g:html_ignore_folding = 1
        endif
        ```

        ### Pandoc markdown example

        ``` {.pandoc .numberLines hl="vim" vars="PhHtmlEngine=tohtml"}
        ### Pandoc markdown example

        * Item 1
        * Item 2
        ```

        ### List fonts in a shell

        ``` {.shelloutput hl="vim" vars="PhBlockRole=output,PhHtmlEngine=tohtml"}
        ||| ls Deja*
        DejaVuSansMonoForPowerline.bdf     DejaVuSansMonoForPowerline.psfu      DejaVuSansMonoForPowerline.sfd  DejaVuSansMonoForPowerline.txt
        DejaVuSansMonoForPowerline.bdfmap  DejaVuSansMonoForPowerline.psfu.bak  DejaVuSansMonoForPowerline.ttf  DejaVuSansMono-Powerline.otf
        ```
    ```

- Content of .vimrc.pandoc (also contained in the previous listing)

    ```vim
  set nocompatible

  filetype off    " filetype is set by vimhl

  let g:lucius_style = 'light'
  let g:lucius_contrast = 'high'
  let g:lucius_contrast_bg = 'high'

  colorscheme lucius
  syntax on

  if !exists('g:PhHtmlPreAttrs')
      let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; background: #FFE"'
  endif

  if exists('g:PhBlockRole') && g:PhBlockRole == 'output'
      let g:PhHtmlPreAttrs = 'style="white-space: pre-wrap; '.
              \ 'display: inline-block; border-style: none none none solid; '.
              \ 'border-color: blue; border-width: 15px; padding: 5px 10px"'
      let g:PhTexBlockStyle = 'Leftbar'
  endif

  runtime plugin/publish_helper.vim

  if exists('g:PhHtmlEngine') && g:PhHtmlEngine == 'tohtml'
      runtime plugin/tohtml.vim
      let g:html_no_progress = 1
      let g:html_ignore_folding = 1
  endif
    ```

- Pandoc template for Latex was produced by command

    ```ShellSession
  $ vimhl_latex_tmpl.sh -s FFFF99 -f FF6699 -d > ~/.pandoc/templates/vimhl.latex
    ```

- **HTML document (rendered in Firefox) produced by command**

    ```ShellSession
  $ pandoc --standalone -Fvimhl -o example.html example.md
    ```

----

<p align="center">
  <img border="1" src="../images/images/vimhl-html.png?raw=true" alt="HTML result"/>
</p>

----

- **Pdf document produced by command**

    ```ShellSession
  $ pandoc -Vgeometry:a4paper --template=vimhl -Fvimhl -o example.pdf example.md
    ```

----

<p align="center">
  <img border="1" src="../images/images/vimhl-latex.png?raw=true" alt="PDF result"/>
</p>

----

Thanks to
---------

Christian Brabandt for plugin Colorizer and Xterm2rgb translation functions.

