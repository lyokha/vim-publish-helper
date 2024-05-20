if exists("b:current_syntax")
  finish
endif

if !exists('g:PhShellOutputPrompt')
    let g:PhShellOutputPrompt = '||| '
endif

exe 'syn region soCmdL start="^\s*'.g:PhShellOutputPrompt.'" end="$"'

hi def link soCmdL Statement

" uncomment next line to make output be highlighted with Comment syntax group
"hi! link Normal Comment

let b:current_syntax = "shelloutput"

