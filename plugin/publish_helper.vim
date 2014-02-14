" File: publish_helper.vim
" Author: Alexey Radkov
" Version: 0.8
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

if !exists('g:PhHtmlEngine')
    let g:PhHtmlEngine = ''
endif

if !exists('g:PhHtmlPreAttrs')
    let g:PhHtmlPreAttrs = ''
endif

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

fun! <SID>get_color_under_cursor(bg, ...)
    let layer = a:bg ? "bg" : "fg"
    let attr = synIDattr(synIDtrans(synID(line("."), col("."), 1)), layer)
    if empty(attr) || attr == -1
        if a:0 == 0 || a:1 == 0
            return 'none'
        endif
        let attr = synIDattr(synIDtrans(hlID('Normal')), layer)
    endif
    return <SID>Xterm2rgb256(attr)
endfun

fun! <SID>get_trimmed_range(fst_line, last_line)
    let result = [a:fst_line, a:last_line]
    let linenr = a:fst_line
    while linenr <= a:last_line
        if getline(linenr) =~ '^[[:blank:]\u00A0]*$'
            if linenr == result[0]
                let result[0] += 1
            endif
        else
            let result[1] = linenr
        endif
        let linenr += 1
    endwhile
    return result
endfun

fun! <SID>make_tohtml_code_highlight(fst_line, last_line, ...)
    let range = [a:fst_line, a:last_line]
    if g:PhTrimBlocks
        let range = <SID>get_trimmed_range(a:fst_line, a:last_line)
    endif
    if range[0] > range[1]
        new +set\ nowrap
        call append(0, ['<pre '.g:PhHtmlPreAttrs.'>', '</pre>'])
        delete
        let &ft = 'html'
        return
    endif
    let colors = g:colors_name
    if exists('g:PhColorscheme') && g:PhColorscheme != g:colors_name
        exe "colorscheme ".g:PhColorscheme
    endif
    let g:html_use_css = 0
    if a:0
        let g:html_number_lines = 1
        let g:html_line_ids = 0
    endif
    exe range[0].",".range[1]."TOhtml"
    unlet g:html_use_css
    if a:0
        unlet g:html_number_lines
        unlet g:html_line_ids
    endif
    if exists('g:PhColorscheme') && g:PhColorscheme != colors
        exe "colorscheme ".colors
    endif
    setlocal nowrap
    1;/^<font face=/-1d
    exe "1s/.*/<pre ".g:PhHtmlPreAttrs.">/"
    silent $;?^</font?+1d
    $s/.*/<\/pre>/
    silent %s/<br>$//e
    silent %s/&nbsp;/ /ge
    if a:0 && a:1 >= 0
        let linenr = a:1
        let n_fmt = strlen(string(linenr + range[1] - range[0]))
        call setpos('.', [0, 2, 1, 0])
        let cursor = getpos('.')
        let last_line = line('$')
        while cursor[1] < last_line
            exe "let linecol = printf('>%".n_fmt."d', ".linenr.")"
            silent s/>\s*\d\+/\=linecol/e
            let cursor[1] += 1
            let linenr += 1
            call setpos('.', cursor)
        endwhile
    endif
    normal gggJ0
endfun

fun! <SID>escape_tex(block)
    let block = a:block
    let block = escape(block, '\{}_$%')
    let block = substitute(block, '\\\\', '\\textbackslash{}', 'g')
    return block
endfun

fun! <SID>escape_html(block)
    let block = a:block
    let block = substitute(block, '&', '\&amp;',  'g')
    let block = substitute(block, '<', '\&lt;',   'g')
    let block = substitute(block, '>', '\&gt;',   'g')
    let block = substitute(block, '"', '\&quot;', 'g')
    return block
endfun

fun! <SID>add_synid(result, synId, line, linenr, fg, bg, sk_fg, sk_bg)
    if g:PhCtrlTrans == 0 || empty(a:sk_fg)
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
            call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strtrans(strpart(a:line, pos, 1)),
                    \  'line': a:linenr, 'fg': a:sk_fg, 'bg': a:sk_bg})
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

fun! <SID>split_synids(fst_line, last_line, ...)
    let result = []
    let save_winview = winsaveview()
    call setpos('.', [0, a:fst_line, 1, 0])
    let cursor = getpos('.')
    let linenr = a:0 ? (a:1 < 0 ? a:fst_line : a:1) : -1
    let n_fmt = strlen(string(linenr + a:last_line - a:fst_line))
    let trans = synIDtrans(hlID('SpecialKey'))
    let sk_fg = toupper(<SID>Xterm2rgb256(synIDattr(trans, 'fg')))
    let sk_bg = toupper(<SID>Xterm2rgb256(synIDattr(trans, 'bg')))
    while cursor[1] <= a:last_line
        let old_synId = '^'
        let old_start = cursor[2]
        let cols = col('$')
        if linenr >= 0
            exe "let linecol = printf('%".n_fmt."d  ', ".linenr.")"
            call <SID>add_synid(result, 'linenr', linecol, line('.'),
                        \ sk_fg, sk_bg, '', '')
        endif
        if cols == 1
            let cursor[1] += 1
            let cursor[2] = 1
            if linenr >= 0
                let linenr += 1
            endif
            call setpos('.', cursor)
            continue
        endif
        let line = getline('.')
        while cursor[2] <= cols
            let synId = synIDattr(synID(line('.'), col('.'), 1), 'name')
            let fg = toupper(<SID>get_color_under_cursor(0, 1))
            let bg = toupper(<SID>get_color_under_cursor(1, 1))
            let cursor[2] += 1
            call setpos('.', cursor)
            if synId != old_synId
                if old_synId != '^'
                    call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_fg, old_bg, sk_fg, sk_bg)
                endif
                let old_synId = synId
                let old_start = cursor[2] - 1
            endif
            let old_fg = fg
            let old_bg = bg
        endwhile
        call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_fg, old_bg, sk_fg, sk_bg)
        let cursor[1] += 1
        let cursor[2] = 1
        if linenr >= 0
            let linenr += 1
        endif
        call setpos('.', cursor)
    endwhile
    call winrestview(save_winview)
    return result
endfun

fun! <SID>make_code_highlight(fst_line, last_line, ft, ...)
    let range = [a:fst_line, a:last_line]
    if g:PhTrimBlocks
        let range = <SID>get_trimmed_range(a:fst_line, a:last_line)
    endif
    let parts = []
    if range[0] <= range[1]
        let colors = g:colors_name
        if exists('g:PhColorscheme') && g:PhColorscheme != g:colors_name
            exe "colorscheme ".g:PhColorscheme
        endif
        if a:0 && a:ft == 'html'
            let parts = call('<SID>split_synids', range + a:000)
        else
            let parts = <SID>split_synids(range[0], range[1])
        endif
        if exists('g:PhColorscheme') && g:PhColorscheme != colors
            exe "colorscheme ".colors
        endif
    endif
    new +set\ nowrap
    if a:ft == 'tex'
        let numbers = ''
        if a:0
            let numbers = "numbers=left,firstnumber=".
                        \ (a:1 < 0 ? range[0] : a:1)
        endif
        call append(0, ['\begin{Shaded}',
                    \ '\begin{Highlighting}['.numbers.']'])
    elseif a:ft == 'html'
        call append(0, '<pre '.g:PhHtmlPreAttrs.'>')
    endif
    delete
    let old_line = range[0]
    let line = old_line
    let content = ''
    for hl in parts
        let line = hl['line']
        while line > old_line
            call append('$', content)
            let old_line += 1
            let content = ''
        endwhile
        let part = hl['content']
        let fg = hl['fg']
        if part !~ '^[[:blank:]\u00A0]*$'
            if a:ft == 'tex'
                let part = <SID>escape_tex(part)
                let part = '\textcolor[HTML]{'.fg.'}{'.part.'}'
            elseif a:ft == 'html'
                let part = <SID>escape_html(part)
                let part = '<span style="color: #'.fg.'">'.part.'</span>'
            endif
        endif
        let content .= part
    endfor
    call append('$', content)
    if !g:PhTrimBlocks
        while line < a:last_line
            call append('$', '')
            let line += 1
        endwhile
    endif
    if a:ft == 'tex'
        call append('$', ['\end{Highlighting}', '\end{Shaded}'])
        call setpos('.', [0, 1, 1, 0])
    elseif a:ft == 'html'
        call append('$', '</pre>')
        normal gggJ0
    endif
    let &ft = a:ft
endfun

fun! <SID>make_tex_code_highlight(fst_line, last_line, ...)
    call call(function('<SID>make_code_highlight'),
                \ [a:fst_line, a:last_line, 'tex'] + a:000)
endfun

fun! <SID>make_html_code_highlight(fst_line, last_line, ...)
    if g:PhHtmlEngine == 'tohtml'
        call call(function('<SID>make_tohtml_code_highlight'),
                    \ [a:fst_line, a:last_line] + a:000)
    else
        call call(function('<SID>make_code_highlight'),
                    \ [a:fst_line, a:last_line, 'html'] + a:000)
    endif
endfun


command -range=% -nargs=? MakeHtmlCodeHighlight silent call
            \ <SID>make_html_code_highlight(<line1>, <line2>, <f-args>)
command -range=% -nargs=? MakeTexCodeHighlight silent call
            \ <SID>make_tex_code_highlight(<line1>, <line2>, <f-args>)

command GetFgColorUnderCursor echo <SID>get_color_under_cursor(0)
command GetBgColorUnderCursor echo <SID>get_color_under_cursor(1)

