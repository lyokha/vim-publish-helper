#!/bin/bash

HL_M_PTN='^\$highlighting-macros\$$'
HL_M_IF_PTN='^\$if(highlighting-macros)\$$'
VERB_N_IF_PTN='^\$if(verbatim-in-note)\$$'
VERB_N_IF2_PTN='^\$if(verbatim-in-note)\$\n\\usepackage{fancyvrb}'
ENDIF_PTN='^\$endif\$$'

BG_COLOR='FFFFEE'
S_BG_COLOR='FFFFEE'
F_COLOR='000000'
BG_COLOR_FMT='HTML'
S_BG_COLOR_FMT='HTML'
F_COLOR_FMT='HTML'

ROUND_CORNER='0pt'
SCRIPTSIZE='
    \\scriptsize\'

while getopts ':mb:s:f:r:nh' opt ; do
    case $opt in
        m) MDFRAMED=1 ;;
        b) BG_COLOR=$OPTARG ;;
        s) S_BG_COLOR=$OPTARG ;;
        f) MDFRAMED=1; F_COLOR=$OPTARG ;;
        r) MDFRAMED=1; ROUND_CORNER=$OPTARG ;;
        n) SCRIPTSIZE= ;;
        h) cat <<END
Prints to STDOUT Pandoc template for Latex compatible with vimhl;
the template defines new environments: Shaded, Snugshade, Framed, Leftbar
and Mdframed (if option -m was specified)

Options:

  -m define environment Mdframed which provides more parameters for
     frames than standard latex package Framed does
  -b set background color in Shaded and Mdframed code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$BG_COLOR'
  -s set background color in Snugshade code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$S_BG_COLOR'
  -f set frame line color in Mdframed code blocks, implies option -m;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$F_COLOR'
  -r set frame round corners magnitude, implies option -m;
     default value is '$ROUND_CORNER'
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

for i in 'BG_' 'S_BG_' 'F_' ; do
    color=$(eval echo $`echo ${i}COLOR`)
    if [[ $color == *,* ]] ; then
        fmt='RGB'
        [[ $color == *.* ]] && fmt='rgb'
        eval `echo ${i}COLOR_FMT`=$fmt
    fi
done

IFS='' read -r -d '' RPL <<END
\\\\usepackage{xcolor}\\
\\\\usepackage{fancyvrb}\\
\\\\newcommand{\\\\VerbBar}{|}\\
\\\\newcommand{\\\\VERB}{\\\\Verb[commandchars=\\\\\\\\\\\\{\\\\}]}\\
\\\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\\\\\\\\\\{\\\\}}\\
\\\\usepackage{framed}\\
\\\\newenvironment{Shaded}{\\
  \\\\definecolor{shadecolor}{$BG_COLOR_FMT}{$BG_COLOR}\\
  \\\\setlength\\\\parskip{0cm}\\
  \\\\setlength\\\\partopsep{-\\\\topsep}\\
  \\\\addtolength\\\\partopsep{0.2cm}\\
  \\\\begin{shaded}\\$SCRIPTSIZE
}{\\\\end{shaded}}\\
\\\\newenvironment{Snugshade}{\\
  \\\\definecolor{shadecolor}{$S_BG_COLOR_FMT}{$S_BG_COLOR}\\
  \\\\begin{snugshade}\\$SCRIPTSIZE
}{\\\\end{snugshade}}\\
\\\\newenvironment{Framed}{\\
  \\\\setlength\\\\parskip{0cm}\\
  \\\\setlength\\\\partopsep{-\\\\topsep}\\
  \\\\addtolength\\\\partopsep{0.2cm}\\
  \\\\begin{framed}\\$SCRIPTSIZE
}{\\\\end{framed}}\\
\\\\newenvironment{Leftbar}{\\
  \\\\setlength\\\\parskip{0cm}\\
  \\\\setlength\\\\partopsep{-\\\\topsep}\\
  \\\\addtolength\\\\partopsep{0.2cm}\\
  \\\\begin{leftbar}\\$SCRIPTSIZE
}{\\\\end{leftbar}}\\
END

IFS='' read -r -d '' MRPL <<END
\\\\usepackage[framemethod=tikz]{mdframed}\\
\\\\newenvironment{Mdframed}{\\
  \\\\definecolor{mdframedbgcolor}{$BG_COLOR_FMT}{$BG_COLOR}\\
  \\\\definecolor{mdframedlcolor}{$F_COLOR_FMT}{$F_COLOR}\\
  \\\\begin{mdframed}[linecolor=mdframedlcolor,\\
                   backgroundcolor=mdframedbgcolor,\\
                   roundcorner=$ROUND_CORNER]\\$SCRIPTSIZE
}{\\\\end{mdframed}}\\
END

[ -n "$MDFRAMED" ] && RPL=$RPL$MRPL

pandoc -D latex |
sed -e "/$HL_M_PTN/i \\$RPL" \
    -e "/$HL_M_IF_PTN/,/$ENDIF_PTN/d" \
    -e "/$VERB_N_IF_PTN/N;/$VERB_N_IF2_PTN/,/$ENDIF_PTN/d"

