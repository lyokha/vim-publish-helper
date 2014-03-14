#!/bin/bash 

HL_M_PTN='^\$highlighting-macros\$$'
HL_M_IF_PTN='^\$if(highlighting-macros)\$$'
VERB_N_IF_PTN='^\$if(verbatim-in-note)\$$'
VERB_N_IF2_PTN='^\$if(verbatim-in-note)\$\n\\usepackage'
ENDIF_PTN='^\$endif\$$'

BG_COLOR='FFFFEE'
COLOR_FMT='HTML'
SCRIPTSIZE='
    \\scriptsize\'
SHAPE='shaded'

while getopts ':b:s:nh' opt ; do
    case $opt in
        b) BG_COLOR=$OPTARG ;;
        s) SHAPE=$OPTARG ;;
        n) SCRIPTSIZE= ;;
        h) cat <<END
Prints to STDOUT Pandoc template for Latex compatible with vimhl

  -s set shape of code blocks;
     accepts any value defined in Latex package Framed, e.g.
     shaded (default value), framed, leftbar etc.
  -b set background color in shaded code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$BG_COLOR'
  -n do not set scriptsize (which is set by default) in code blocks
  -h print this message and exit

END
           exit 0 ;;
       \?) echo "Invalid option: -$OPTARG" >&2
           exit 1 ;;
        :) echo "Option -$OPTARG requires an argument" >&2
           exit 1 ;;
    esac
done

shift $((OPTIND-1))

if [[ $BG_COLOR == *,* ]] ; then
    COLOR_FMT='RGB'
    if [[ $BG_COLOR == *.* ]] ; then
        COLOR_FMT='rgb'
    fi
fi

IFS='' read -r -d '' RPL <<END
\\\\usepackage{xcolor}\\
\\\\usepackage{fancyvrb}\\
\\\\newcommand{\\\\VerbBar}{|}\\
\\\\newcommand{\\\\VERB}{\\\\Verb[commandchars=\\\\\\\\\\\\{\\\\}]}\\
\\\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\\\\\\\\\\{\\\\}}\\
\\\\usepackage{framed}\\
\\\\newenvironment{Shaded}{\\
  \\\\definecolor{shadecolor}{$COLOR_FMT}{$BG_COLOR}\\
  \\\\setlength\\\\parskip{0cm}\\
  \\\\setlength\\\\partopsep{-\\\\topsep}\\
  \\\\addtolength\\\\partopsep{0.2cm}\\
  \\\\begin{$SHAPE}\\$SCRIPTSIZE
}{\\\\end{$SHAPE}}
END

pandoc -D latex |
sed -e "/$HL_M_PTN/i \\$RPL" \
    -e "/$HL_M_IF_PTN/,/$ENDIF_PTN/d" \
    -e "/$VERB_N_IF_PTN/N;/$VERB_N_IF2_PTN/,/$ENDIF_PTN/d"

