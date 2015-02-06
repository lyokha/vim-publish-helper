" File: publish_helper.vim
" Author: Alexey Radkov
" Version: 0.12
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

if !exists('g:PhLinenrColumnAttrs')
    let g:PhLinenrColumnAttrs = ''
endif

if !exists('g:PhCodeColumnOverflowX')
    let g:PhCodeColumnOverflowX = 'auto'
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
    let layer = a:bg ? 'bg' : 'fg'
    let trans = a:0 > 1 ? a:2 : synIDtrans(synID(line('.'), col('.'), 1))
    let attr = synIDattr(trans, layer)
    if empty(attr) || attr == -1
        if a:0 == 0 || a:1 == 0
            return 'none'
        endif
        let attr = synIDattr(synIDtrans(hlID('Normal')), layer)
    endif
    if attr =~ '^#'
        return substitute(attr, '^#', '', '')
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
    let ft = &ft
    let save_globals = {}
    for var in ['g:html_use_css', 'g:html_number_lines', 'g:html_line_ids']
        exe 'let val = '.(exists(var) ? var : '""')
        let save_globals[var] = exists(var) ? 'let '.var.' = '.val :
                    \ 'unlet '.var
    endfor
    let g:html_use_css = 0
    let g:html_number_lines = a:0 ? 1 : 0
    let g:html_line_ids = 0
    exe range[0].",".range[1]."TOhtml"
    for val in values(save_globals)
        exe val
    endfor
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
    if ft == g:PhShellOutputFt
        exe 'silent %s/>\s*\zs'.g:PhShellOutputPrompt.'//e'
    endif
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

fun! <SID>add_synid(result, synId, line, linenr, trans, sk_trans)
    if g:PhCtrlTrans == 0 || a:sk_trans == -1
        call add(a:result,
                    \ {'name': a:synId, 'content': a:line,
                    \  'line': a:linenr, 'trans': a:trans})
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
                    \  'line': a:linenr, 'trans': a:trans})
            endif
            call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strtrans(strpart(a:line, pos, 1)),
                    \  'line': a:linenr, 'trans': a:sk_trans})
            let pos += 1
        endif
    endwhile
    if old_pos < len && pos == -1
        call add(a:result,
                    \ {'name': a:synId,
                    \  'content': strpart(a:line, old_pos, len - old_pos),
                    \  'line': a:linenr, 'trans': a:trans})
    endif
endfun

fun! <SID>split_synids(fst_line, last_line, ...)
    let result = []
    let save_winview = winsaveview()
    call setpos('.', [0, a:fst_line, 1, 0])
    let cursor = getpos('.')
    let linenr = a:0 ? (a:1 < 0 ? a:fst_line : a:1) : -1
    let n_fmt = strlen(string(linenr + a:last_line - a:fst_line))
    let sk_trans = synIDtrans(hlID('SpecialKey'))
    while cursor[1] <= a:last_line
        let old_synId = '^'
        let old_start = cursor[2]
        let cols = col('$')
        if linenr >= 0
            let linecol = linenr
            if !g:PhLinenrAsTblColumn
                exe "let linecol = printf('%".n_fmt."d  ', ".linenr.")"
            endif
            call <SID>add_synid(result, 'linenr', linecol, line('.'),
                        \ sk_trans, -1)
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
            let synIdNmb = synID(line('.'), col('.'), 1)
            let synId = synIDattr(synIdNmb, 'name')
            let trans = synIDtrans(synIdNmb)
            let cursor[2] += 1
            call setpos('.', cursor)
            if synId != old_synId
                if old_synId != '^'
                    call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_trans, sk_trans)
                endif
                let old_synId = synId
                let old_start = cursor[2] - 1
            endif
            let old_trans = trans
        endwhile
        call <SID>add_synid(result, old_synId,
                    \ strpart(line, old_start - 1, cursor[2] - old_start - 1),
                    \ line('.'), old_trans, sk_trans)
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

fun! <SID>add_tex_rich_elem(part, trans, fg, elems, idx)
    if a:idx == len(a:elems)
        return a:part
    endif
    let nested = <SID>add_tex_rich_elem(a:part, a:trans, a:fg, a:elems,
                \ a:idx + 1)
    if a:elems[a:idx] == 'fg'
        return '\textcolor[HTML]{'.a:fg.'}{'.nested.'}'
    endif
    if a:elems[a:idx] == 'bg'
        let bg = toupper(<SID>get_color_under_cursor(1, 0, a:trans))
        if bg == 'NONE'
            return nested
        endif
        return '\colorbox[HTML]{'.bg.'}{'.nested.'}'
    endif
    if a:elems[a:idx] == 'bold'
        return synIDattr(a:trans, 'bold') == 1 ?
                    \ '\textbf{'.nested.'}' : nested
    endif
    if a:elems[a:idx] == 'italic'
        return synIDattr(a:trans, 'italic') == 1 ?
                    \  '\emph{'.nested.'}' : nested
    endif
    if a:elems[a:idx] == 'underline'
        return synIDattr(a:trans, 'underline') == 1 ?
                    \ '\underline{'.nested.'}' : nested
    endif
    return nested
endfun

fun! <SID>sort_tex_rich_elems(e1, e2)
    let order = ['bg', 'fg', 'bold', 'italic', 'underline']
    return index(order, a:e2) < index(order, a:e1)
endfun

fun! <SID>add_tex_rich_elems(part, trans, fg)
    let elems = sort(g:PhRichTextElems + ['fg'], '<SID>sort_tex_rich_elems')
    return <SID>add_tex_rich_elem(a:part, a:trans, a:fg, elems, 0)
endfun

fun! <SID>add_html_rich_elem(elem, trans)
    if a:elem == 'bg'
        let bg = toupper(<SID>get_color_under_cursor(1, 0, a:trans))
        if bg == 'NONE'
            return ''
        endif
        return ' background-color: #'.bg.';'
    endif
    if synIDattr(a:trans, a:elem) == 1
        if a:elem == 'bold'
            return ' font-weight: bold;'
        endif
        if a:elem == 'italic'
            return ' font-style: italic;'
        endif
        if a:elem == 'underline'
            return ' text-decoration: underline;'
        endif
    endif
    return ''
endfun

fun! <SID>linenr_tbl_layout(attrs)
    let div_style = '<div style="overflow-x: '.g:PhCodeColumnOverflowX.';">'
    let div_style_end = '</div>'
    let pre_style = substitute(a:attrs, '\<style="',
                \ 'style="display: inline; ', '')
    if pre_style == a:attrs
        let pre_style = a:attrs.' style="display: inline;"'
    endif
    let div_style_test = matchstr(a:attrs,
                \ '\<style="[^"]*\zs\<background:[^;"]*;\?')
    if !empty(div_style_test)
        let div_style = '<div style="overflow-x: '.g:PhCodeColumnOverflowX.
                    \ '; '.div_style_test.'">'
    endif
    return {'pre': pre_style, 'div': div_style, 'div_end': div_style_end}
endfun

fun! <SID>make_code_highlight(fst_line, last_line, ft, ...)
    let range = [a:fst_line, a:last_line]
    if g:PhTrimBlocks
        let range = <SID>get_trimmed_range(a:fst_line, a:last_line)
    endif
    let shell_output_tex = &ft == g:PhShellOutputFt && a:ft == 'tex'
    let shell_output_html = &ft == g:PhShellOutputFt && a:ft == 'html'
    let linenr_html = a:0 && a:ft == 'html'
    let linenr_html_tbl = linenr_html && g:PhLinenrAsTblColumn
    let parts = []
    if range[0] <= range[1]
        if shell_output_tex
            let parts = getline(range[0], range[1])
        else
            let colors = g:colors_name
            if exists('g:PhColorscheme') && g:PhColorscheme != g:colors_name
                exe "colorscheme ".g:PhColorscheme
            endif
            if linenr_html
                let parts = call('<SID>split_synids', range + a:000)
            else
                let parts = <SID>split_synids(range[0], range[1])
            endif
            if exists('g:PhColorscheme') && g:PhColorscheme != colors
                exe "colorscheme ".colors
            endif
        endif
    endif
    new +set\ nowrap
    if shell_output_tex
        let alsoletter = empty(g:PhAlsoletter) ? '' :
                    \ ',alsoletter={'.g:PhAlsoletter.'}'
        call append(0, ['\begin{'.g:PhTexBlockStyle.'}',
                    \ '\begin{lstlisting}[language='.g:PhShellOutputFt.
                    \ ',breaklines'.alsoletter.']'])
    elseif a:ft == 'tex'
        let numbers = ''
        if a:0
            let numbers = "numbers=left,firstnumber=".
                        \ (a:1 < 0 ? range[0] : a:1)
        endif
        call append(0, ['\begin{'.g:PhTexBlockStyle.'}',
                    \ '\begin{Highlighting}['.numbers.']'])
    endif
    delete
    let line = range[0]
    if shell_output_tex
        for part in parts
            call append('$', part)
            let line += 1
        endfor
    else
        let pre_style = g:PhHtmlPreAttrs
        let div_style = ''
        let div_style_end = ''
        if linenr_html_tbl
            let fg = '000000'
            if !exists('g:PhLinenrFgColor')
                let attr = synIDattr(synIDtrans(hlID('SpecialKey')), 'fg')
                let fg = toupper(<SID>Xterm2rgb256(attr))
            else
                let fg = toupper(g:PhLinenrFgColor)
            endif
            let styles = <SID>linenr_tbl_layout(g:PhLinenrColumnAttrs)
            let pre_style = styles['pre']
            let div_style = styles['div']
            let div_style_end = styles['div_end']
            call append(0, '<table style="margin: 0; border-spacing: 0; '.
                        \ 'width: 100%; table-layout: fixed; border: none;">'.
                        \ '<tr><td style="vertical-align: top; width: '.
                        \ g:PhLinenrColumnWidth.'; text-align: right; '.
                        \ 'padding-right: 3px; color: #'.fg.
                        \ '; border-right: '.g:PhLinenrColumnBorderAttrs.' #'.
                        \ fg.';">'.div_style.'<pre '.pre_style.'>')
            for hl in parts
                if hl['name'] == 'linenr'
                    call append('$', hl['content'])
                endif
            endfor
            call append('$', '</pre>'.div_style_end.
                        \ '</td><td style="vertical-align: top; '.
                        \ 'padding-left: 2px">')
            let styles = <SID>linenr_tbl_layout(g:PhHtmlPreAttrs)
            let pre_style = styles['pre']
            let div_style = styles['div']
            let div_style_end = styles['div_end']
        endif
        if a:ft == 'html'
            call append(linenr_html_tbl ? '$' : 0,
                        \ div_style.'<pre '.pre_style.'>')
        endif
        let old_line = line
        let content = ''
        for hl in parts
            let line = hl['line']
            while line > old_line
                call append('$', content)
                let old_line += 1
                let content = ''
            endwhile
            let part = hl['content']
            let trans = hl['trans']
            let fg = toupper(<SID>get_color_under_cursor(0, 1, trans))
            if part !~ '^[[:blank:]\u00A0]*$'
                if a:ft == 'tex'
                    let part = <SID>escape_tex(part)
                    let part = <SID>add_tex_rich_elems(part, trans, fg)
                elseif a:ft == 'html'
                    if shell_output_html
                        exe 'let part = substitute(part, ''^\s*\zs'
                                    \ .g:PhShellOutputPrompt.''', "", "")'
                    endif
                    let value = <SID>escape_html(part)
                    let part = '<span style="color: #'.fg.';'
                    for elem in g:PhRichTextElems
                        let part .= <SID>add_html_rich_elem(elem, trans)
                    endfor
                    let part .= '">'.value.'</span>'
                endif
            endif
            if !(linenr_html_tbl && hl['name'] == 'linenr')
                let content .= part
            endif
        endfor
        call append('$', content)
    endif
    if !g:PhTrimBlocks
        while line < a:last_line
            call append('$', '')
            let line += 1
        endwhile
    endif
    if shell_output_tex
        call append('$', ['\end{lstlisting}',
                    \ '\end{'.g:PhTexBlockStyle.'}'])
        call setpos('.', [0, 1, 1, 0])
    elseif a:ft == 'tex'
        call append('$', ['\end{Highlighting}',
                    \ '\end{'.g:PhTexBlockStyle.'}'])
        call setpos('.', [0, 1, 1, 0])
    elseif a:ft == 'html'
        call append('$', '</pre>')
        if linenr_html_tbl
            call append('$', div_style_end.'</td></tr></table>')
        endif
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

