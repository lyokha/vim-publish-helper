" File: plugin/publish_helper.vim
" Author: Alexey Radkov
" Version: 0.16
" Description: two commands for publishing highlighted code in HTML or TeX
"              (optionally from pandoc as highlighting engine from filter
"              vimhl.hs)
"
" Usage:
"   The commands may be used from within vim. Additionally they can act as
"   the highlighting engine from pandoc having been inserted inside filter
"   vimhl.hs shipped with this distribution.
"
"   Command :MakeHtmlCodeHighlight to create a new buffer in a new window with
"   HTML code correctly highlighted accordingly to the current color scheme or
"   scheme declared in variable g:PhColorscheme. The command uses TOhtml
"   internally with temporarily set value g:html_use_css = 0
"
"   Command :MakeTexCodeHighlight to create a new buffer in a new window with
"   TeX code correctly highlighted accordingly to the current color scheme or
"   scheme declared in variable g:PhColorscheme. The command accepts optional
"   argument which defines that lines will be enumerated. If the argument is
"   missing then no line enumeration will be generated, if it is negative then
"   lines will be enumerated from the number of the first line in the original
"   buffer, otherwise lines will be enumerated from the value of the argument.
"
"   The both commands can act as the highlighting engine in filter vimhl.hs
"   shipped with this distribution. This program is supposed to be used like
"
"   pandoc -f html -t latex -F vimhl.hs -o document.tex document.html
"
"   It is possible to compile vimhl.hs:
"
"   ghc --make vimhl
"
"   Move vimhl.hs (or compiled program vimhl) somewhere in your $PATH.
"
"   When converting formats with pandoc ensure that original file has
"   additional parameter hl="vim" inside tag <pre> (as an example for HTML):
"   only in this case pandoc will choose vimhl.hs as highlighting filter.
"
"   The filter was tested only for HTML to TeX conversions: it may contain
"   bugs for other cases!
"
"   There are two other commands :GetFgColorUnderCursor and
"   :GetBgColorUnderCursor. They are included as they may help in debugging
"   and finding foreground or background colors under cursor. Normally i use
"   them in maps like following:
"
"   nmap <silent> ,vf :GetFgColorUnderCursor<CR>
"   nmap <silent> ,vb :GetBgColorUnderCursor<CR>
"
" Thanks: Christian Brabandt for plugin Colorizer and Xterm2rgb translation
"         functions

if exists('g:loaded_PublishHelperPlugin') && g:loaded_PublishHelperPlugin
    finish
endif

let g:loaded_PublishHelperPlugin = 1

if !exists('g:PhCtrlTrans')
    let g:PhCtrlTrans = 0
endif

if !exists('g:PhTrimBlocks')
    let g:PhTrimBlocks = 1
endif

if !exists('g:PhHighlightEngine')
    let g:PhHighlightEngine = ''
endif

if !exists('g:PhHtmlEngine')
    let g:PhHtmlEngine = ''
endif

if !exists('g:PhHtmlPreAttrs')
    let g:PhHtmlPreAttrs = ''
endif

if !exists('g:PhTexBlockStyle')
    let g:PhTexBlockStyle = 'Shaded'
endif

if !exists('g:PhShellOutputFt')
    let g:PhShellOutputFt = 'shelloutput'
endif

if !exists('g:PhShellOutputPrompt')
    let g:PhShellOutputPrompt = '||| '
endif

if !exists('g:PhAlsoletter')
    let g:PhAlsoletter = ''
endif

if !exists('g:PhRichTextElems')
    " accepted elements are 'bg', 'bold', 'italic' and 'underline'
    let g:PhRichTextElems = ['bg', 'bold', 'italic', 'underline']
endif

if !exists('g:PhLinenrAsTblColumn')
    let g:PhLinenrAsTblColumn = 0
endif

if !exists('g:PhLinenrColumnBorderAttrs')
    let g:PhLinenrColumnBorderAttrs = '1px solid'
endif

if !exists('g:PhLinenrColumnWidth')
    let g:PhLinenrColumnWidth = '2em'
endif

if !exists('g:PhLinenrTblBgColor')
    let g:PhLinenrTblBgColor = 'inherit'
endif

if !exists('g:PhLinenrTblBorderSpacing')
    let g:PhLinenrTblBorderSpacing = '0'
endif

if !exists('g:PhLinenrTblBottomPadding')
    let g:PhLinenrTblBottomPadding = '0'
endif

if !exists('g:PhLinenrColumnAttrs')
    let g:PhLinenrColumnAttrs = ''
endif

if !exists('g:PhCodeColumnOverflowX')
    let g:PhCodeColumnOverflowX = 'auto'
endif


command -range=% -nargs=? MakeHtmlCodeHighlight silent call
            \ publish_helper#make_html_code_highlight(<line1>, <line2>,
            \ <f-args>)
command -range=% -nargs=? MakeTexCodeHighlight silent call
            \ publish_helper#make_tex_code_highlight(<line1>, <line2>,
            \ <f-args>)

command GetFgColorUnderCursor echo publish_helper#get_color_under_cursor(0,
            \ g:PhHighlightEngine == 'treesitter')
command GetBgColorUnderCursor echo publish_helper#get_color_under_cursor(1,
            \ g:PhHighlightEngine == 'treesitter')

