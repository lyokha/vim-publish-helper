if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if !exists('g:PhShellOutputPrompt')
    let g:PhShellOutputPrompt = '||| '
endif

exe 'syn region soCmdL start="^\s*'.g:PhShellOutputPrompt.'" end="$"'

hi def link soCmdL Statement

