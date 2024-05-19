---
geometry: left=3cm,right=3cm,top=3cm,bottom=3cm
...

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

