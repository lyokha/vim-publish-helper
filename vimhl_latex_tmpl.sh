#!/bin/bash 

HL_M_PTN='^\$highlighting-macros\$$'
HL_M_IF_PTN='^\$if(highlighting-macros)\$$'
VERB_N_IF_PTN='^\$if(verbatim-in-note)\$$'
VERB_N_IF2_PTN='^\$if(verbatim-in-note)\$\n\\usepackage'
ENDIF_PTN='^\$endif\$$'

BG_COLOR='FFFFEE'
S_BG_COLOR='FFFFEE'
COLOR_FMT='HTML'
S_COLOR_FMT='HTML'
SCRIPTSIZE='
    \\scriptsize\'

while getopts ':b:s:nh' opt ; do
    case $opt in
        b) BG_COLOR=$OPTARG ;;
        s) S_BG_COLOR=$OPTARG ;;
        n) SCRIPTSIZE= ;;
        h) cat <<END
Prints to STDOUT Pandoc template for Latex compatible with vimhl;
the template defines new environments: Shaded, Snugshade, Framed and Leftbar

  -b set background color in shaded code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$BG_COLOR'
  -s set background color in snugshade code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$S_BG_COLOR'
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

if [[ $S_BG_COLOR == *,* ]] ; then
    S_COLOR_FMT='RGB'
    if [[ $S_BG_COLOR == *.* ]] ; then
        S_COLOR_FMT='rgb'
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
  \\\\begin{shaded}\\$SCRIPTSIZE
}{\\\\end{shaded}}\\
\\\\newenvironment{Snugshade}{\\
  \\\\definecolor{shadecolor}{$S_COLOR_FMT}{$S_BG_COLOR}\\
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
  \\\\setlength{\\\\FrameSep}{10cm}\\
  \\\\begin{leftbar}\\$SCRIPTSIZE
}{\\\\end{leftbar}}
END

pandoc -D latex |
sed -e "/$HL_M_PTN/i \\$RPL" \
    -e "/$HL_M_IF_PTN/,/$ENDIF_PTN/d" \
    -e "/$VERB_N_IF_PTN/N;/$VERB_N_IF2_PTN/,/$ENDIF_PTN/d"

