" File: publish_helper.vim
" Author: Alexey Radkov
" Version: 0.5
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
"   nmap <silent> ,vc :GetFgColorUnderCursor<CR>
"   nmap <silent> ,vb :GetBgColorUnderCursor<CR>
"
" Issues: tex code maker does not work properly with control characters.
"
" Thanks: Christian Brabandt for plugin Colorizer and Xterm2rgb translation
"         functions

if exists('g:loaded_PublishHelperPlugin') && g:loaded_PublishHelperPlugin
    finish
endif

let g:loaded_PublishHelperPlugin = 1

" next Xterm2rgb... conversion functions are adopted from plugin Colorizer.vim
fun! <SID>Xterm2rgb16(color)
    " 16 basic colors
    let r=0
    let g=0
    let b=0
    let basic16 = [
                \ [ 0x00, 0x00, 0x00 ], 
                \ [ 0xCD, 0x00, 0x00 ], 
                \ [ 0x00, 0xCD, 0x00 ], 
                \ [ 0xCD, 0xCD, 0x00 ], 
                \ [ 0x00, 0x00, 0xEE ], 
                \ [ 0xCD, 0x00, 0xCD ], 
                \ [ 0x00, 0xCD, 0xCD ], 
                \ [ 0xE5, 0xE5, 0xE5 ], 
                \ [ 0x7F, 0x7F, 0x7F ], 
                \ [ 0xFF, 0x00, 0x00 ], 
                \ [ 0x00, 0xFF, 0x00 ], 
                \ [ 0xFF, 0xFF, 0x00 ], 
                \ [ 0x5C, 0x5C, 0xFF ], 
                \ [ 0xFF, 0x00, 0xFF ], 
                \ [ 0x00, 0xFF, 0xFF ], 
                \ [ 0xFF, 0xFF, 0xFF ]
                \ ]
    let r = basic16[a:color][0]
    let g = basic16[a:color][1]
    let b = basic16[a:color][2]
    return printf("%02x%02x%02x", r, g, b)
endfun

fun! <SID>Xterm2rgb256(color)
    let r=0
    let g=0
    let b=0
    " 16 basic colors
    if a:color < 16
        return <SID>Xterm2rgb16(a:color)
    " color cube color
    elseif a:color >= 16 && a:color < 232
        " the 6 value iterations in the xterm color cube
        let valuerange6 = [ 0x00, 0x5F, 0x87, 0xAF, 0xD7, 0xFF ]
        let color=a:color-16
        let r = valuerange6[(color/36)%6]
        let g = valuerange6[(color/6)%6]
        let b = valuerange6[color%6]
    " gray tone
    elseif a:color >= 232 && a:color <= 255
        let r = 8 + (a:color-232) * 0x0a
        let g = r
        let b = r
    endif
    return printf("%02x%02x%02x", r, g, b)
endfun

fun! <SID>get_color_under_cursor(bg)
    let synId = synID(line("."), col("."), 1)
    let name = synIDattr(synId, "name")
    if name == ''
        return 'none'
    endif
    let layer = a:bg ? "bg" : "fg"
    let attr = synIDattr(synIDtrans(synId), layer)
    if empty(attr) || attr == -1
        let attr = synIDattr(synIDtrans(hlID('Normal')), layer)
    endif
    return <SID>Xterm2rgb256(attr)
endfun

fun! <SID>make_html_code_highlight(prepare_for_insertion, line1, line2)
    let colors = g:colors_name
    if exists('g:PhColorscheme') && g:PhColorscheme != g:colors_name
        exe "colorscheme ".g:PhColorscheme
    endif
    let g:html_use_css = 0
    exe a:line1.",".a:line2."TOhtml"
    unlet g:html_use_css
    if exists('g:PhColorscheme') && g:PhColorscheme != colors
        exe "colorscheme ".colors
    endif
    setlocal nowrap
    if a:prepare_for_insertion
        1;/^<font face=/-1d
        1s/.*/<pre><tt>/
        silent $;?^</font?+1d
        $s/.*/<\/tt><\/pre>/
        %s/<br>$//
        if getline(2) =~ '^[[:blank:]]*$'
            2;/[^[:blank:]]\+/-1d
        endif
        if getline(line('$') - 1) =~ '^[[:blank:]]*$'
            silent $-1;?[^[:blank:]]\+?+1d
        endif
        normal ggJx0
    endif
endfun

fun! <SID>add_synid(result, synId, line, linenr, fg, bg)
    if !exists('g:PhCtrlTrans') || g:PhCtrlTrans == 0
        call add(a:result,
                    \ {'name': a:synId, 'content': a:line,
                    \  'line': a:linenr, 'fg': a:fg, 'bg': a:bg})
        return
    endif
    let len = strlen(a:line)
    let pos = 0
    let old_pos = 0
    while pos != -1 && pos < len
        let old_pos = pos
        let pos = match(a:line, '[^[:print:]]', pos)
        if pos != -1
            if pos > old_pos
                call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strpart(a:line, old_pos, pos - old_pos),
                    \  'line': a:linenr, 'fg': a:fg, 'bg': a:bg})
            endif
            let trans = synIDtrans(hlID('SpecialKey'))
            let fg = toupper(<SID>Xterm2rgb256(synIDattr(trans, 'fg')))
            let bg = toupper(<SID>Xterm2rgb256(synIDattr(trans, 'bg')))
            call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strtrans(strpart(a:line, pos, 1)),
                    \  'line': a:linenr, 'fg': fg, 'bg': bg})
            let pos += 1
        endif
    endwhile
    if old_pos < len && pos == -1
        call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strpart(a:line, old_pos, len - old_pos),
                    \  'line': a:linenr, 'fg': a:fg, 'bg': a:bg})
    endif
endfun

fun! <SID>split_synids(fst_line, last_line)
    let result = []
    let save_winview = winsaveview()
    call setpos('.', [0, a:fst_line, 1, 0])
    let cursor = getpos('.')
    while cursor[1] <= a:last_line
        let old_synId = '^'
        let old_start = cursor[2]
        let cols = col('$')
        if cols == 1
            let cursor[1] += 1
            let cursor[2] = 1
            call setpos('.', cursor)
            continue
        endif
        let line = getline('.')
        while cursor[2] <= cols
            let synId = synIDattr(synID(line('.'), col('.'), 1), 'name')
            let fg = toupper(<SID>get_color_under_cursor(0))
            let bg = toupper(<SID>get_color_under_cursor(1))
            let cursor[2] += 1
            call setpos('.', cursor)
            if synId != old_synId
                if old_synId != '^'
                    call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_fg, old_bg)
                endif
                let old_synId = synId
                let old_start = cursor[2] - 1
            endif
            let old_fg = fg
            let old_bg = bg
        endwhile
        call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_fg, old_bg)
        let cursor[1] += 1
        let cursor[2] = 1
        call setpos('.', cursor)
    endwhile
    call winrestview(save_winview)
    return result
endfun

fun! <SID>make_tex_code_highlight(fst_line, last_line, ...)
    let colors = g:colors_name
    if exists('g:PhColorscheme') && g:PhColorscheme != g:colors_name
        exe "colorscheme ".g:PhColorscheme
    endif
    let parts = <SID>split_synids(a:fst_line, a:last_line)
    if exists('g:PhColorscheme') && g:PhColorscheme != colors
        exe "colorscheme ".colors
    endif
    let save_paste = &paste
    new +set\ nowrap\ paste
    let numbers = ''
    if a:0
        let numbers = "numbers=left,firstnumber=".(a:1 < 0 ? a:fst_line : a:1)
    endif
    call append(0, ['\begin{Shaded}', '\begin{Highlighting}['.numbers.']'])
    normal dd
    let old_line = a:fst_line
    let line = old_line
    let content = ''
    for hl in parts
        let line = hl['line']
        while line > old_line
            call append('$', content)
            let old_line += 1
            let content = ''
        endwhile
        let part = escape(hl['content'], '\{}_$%')
        let part = substitute(part, '\\\\', '\\textbackslash{}', 'g')
        let fg = hl['fg']
        if fg != 'NONE' && part !~ '^\s*$'
            let part = '\textcolor[HTML]{'.fg.'}{'.part.'}'
        endif
        let content .= part
    endfor
    call append('$', content)
    while line < a:last_line
        call append('$', '')
        let line += 1
    endwhile
    call append('$', ['\end{Highlighting}', '\end{Shaded}'])
    call setpos('.', [0, 1, 1, 0])
    set ft=tex
    if !save_paste
        set nopaste
    endif
endfun


command -range=% MakeHtmlCodeHighlight silent call
            \ <SID>make_html_code_highlight(1, <line1>, <line2>)
command -range=% -nargs=? MakeTexCodeHighlight silent call
            \ <SID>make_tex_code_highlight(<line1>, <line2>, <f-args>)

command GetFgColorUnderCursor echo <SID>get_color_under_cursor(0)
command GetBgColorUnderCursor echo <SID>get_color_under_cursor(1)

