#!/bin/bash

HL_M_PTN='^\$highlighting-macros\$$'
HL_M_IF_PTN='^\$if(highlighting-macros)\$$'
VERB_N_IF_PTN='^\$if(verbatim-in-note)\$$'
VERB_N_IF2_PTN='^\$if(verbatim-in-note)\$\n\\usepackage{fancyvrb}'
LST_IF_PTN='^SKIP THIS$'
LST_IF2_PTN='^\$if(listings)\$\n\\usepackage{listings}'
ENDIF_PTN='^\$endif\$$'

BG_COLOR='FFFFEE'
S_BG_COLOR='FFFFEE'
F_COLOR='000000'
SH_P_COLOR='000000'
SH_O_COLOR='666666'
LB_COLOR='000000'
BG_COLOR_FMT='HTML'
S_BG_COLOR_FMT='HTML'
F_COLOR_FMT='HTML'
SH_P_COLOR_FMT='HTML'
SH_O_COLOR_FMT='HTML'
LB_COLOR_FMT='HTML'

ROUND_CORNER='0pt'
LB_WIDTH='3pt'
SCRIPTSIZE='
    \\scriptsize\'

while getopts ':mb:s:l:w:f:r:dp:o:nch' opt ; do
    case $opt in
        m) MDFRAMED=1 ;;
        b) BG_COLOR=$OPTARG ;;
        s) S_BG_COLOR=$OPTARG ;;
        l) LB_COLOR=$OPTARG ;;
        w) LB_WIDTH=$OPTARG ;;
        f) MDFRAMED=1; F_COLOR=$OPTARG ;;
        r) MDFRAMED=1; ROUND_CORNER=$OPTARG ;;
        d) SHOUTPUT=1 ;;
        p) SHOUTPUT=1; SH_P_COLOR=$OPTARG ;;
        o) SHOUTPUT=1; SH_O_COLOR=$OPTARG ;;
        n) SCRIPTSIZE= ;;
        c) HLCOMPAT=1 ;;
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
  -l set left bar color in Leftbar code blocks;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$LB_COLOR'
  -w set left bar width in Leftbar code blocks;
     default value is '$LB_WIDTH'
  -f set frame line color in Mdframed code blocks, implies option -m;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$F_COLOR'
  -r set frame round corners magnitude, implies option -m;
     default value is '$ROUND_CORNER'
  -d define shell output language for latex package Listings
  -p set prompt color for shell output language, implies option -d;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$SH_P_COLOR'
  -o set output color for shell output language, implies option -d;
     HTML, RGB and rgb (comma-separated values) formats are supported;
     default value is '$SH_O_COLOR'
  -n do not set scriptsize (which is set by default) in code blocks
  -c add declarations required by the original pandoc code
     highlighting engine; this may be useful if a document contains
     parts to be highlighted by that

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

for i in 'BG_' 'S_BG_' 'F_' 'SH_P_' 'SH_O_' 'LB_' ; do
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
  \\\\setlength\\\\fboxsep{1pt}\\
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
  \\\\setlength\\\\fboxsep{1pt}\\
  \\\\begin{framed}\\$SCRIPTSIZE
}{\\\\end{framed}}\\
\\\\renewenvironment{leftbar}{%\\
  \\\\definecolor{leftbarcolor}{$LB_COLOR_FMT}{$LB_COLOR}%\\
  \\\\def\\\\FrameCommand{{\\\\color{leftbarcolor}\\\\vrule width $LB_WIDTH} \\\\hspace{10pt}}%\\
  \\\\MakeFramed {\\\\advance\\\\hsize-\\\\width \\\\FrameRestore}}%\\
 {\\\\endMakeFramed}\\
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
  \\\\setlength\\\\fboxsep{1pt}\\
  \\\\begin{mdframed}[linecolor=mdframedlcolor,\\
                   backgroundcolor=mdframedbgcolor,\\
                   roundcorner=$ROUND_CORNER]\\$SCRIPTSIZE
}{\\\\end{mdframed}}\\
END

IFS='' read -r -d '' DRPL <<END
\\\\usepackage{MnSymbol}\\
\\\\usepackage{listings}\\
\\\\definecolor{shellpromptcolor}{$SH_P_COLOR_FMT}{$SH_P_COLOR}\\
\\\\definecolor{shelloutputcolor}{$SH_O_COLOR_FMT}{$SH_O_COLOR}\\
\\\\lstset{basicstyle=\\\\scriptsize\\\\ttfamily, breaklines=true}\\
\\\\lstset{prebreak=\\\\raisebox{0ex}[0ex][0ex]\\
  {\\\\ensuremath{\\\\rhookswarrow}}}\\
\\\\lstset{postbreak=\\\\raisebox{0ex}[0ex][0ex]\\
  {\\\\ensuremath{\\\\rcurvearrowse\\\\space}}}\\
\\\\lstdefinelanguage{shelloutput}\\
  {basicstyle=\\\\color{shelloutputcolor}\\$SCRIPTSIZE
    \\\\ttfamily\\\\itshape,\\
   moredelim=[il][\\\\color{shellpromptcolor}\\\\upshape]{|||\\\\ }}\\
END

IFS='' read -r -d '' CRPL <<END
\\\\newcommand{\\\\KeywordTok}[1]{\\\\textcolor[rgb]{0.00,0.44,0.13}{\\\\textbf{{#1}}}}\\
\\\\newcommand{\\\\DataTypeTok}[1]{\\\\textcolor[rgb]{0.56,0.13,0.00}{{#1}}}\\
\\\\newcommand{\\\\DecValTok}[1]{\\\\textcolor[rgb]{0.25,0.63,0.44}{{#1}}}\\
\\\\newcommand{\\\\BaseNTok}[1]{\\\\textcolor[rgb]{0.25,0.63,0.44}{{#1}}}\\
\\\\newcommand{\\\\FloatTok}[1]{\\\\textcolor[rgb]{0.25,0.63,0.44}{{#1}}}\\
\\\\newcommand{\\\\CharTok}[1]{\\\\textcolor[rgb]{0.25,0.44,0.63}{{#1}}}\\
\\\\newcommand{\\\\StringTok}[1]{\\\\textcolor[rgb]{0.25,0.44,0.63}{{#1}}}\\
\\\\newcommand{\\\\CommentTok}[1]{\\\\textcolor[rgb]{0.38,0.63,0.69}{\\\\textit{{#1}}}}\\
\\\\newcommand{\\\\OtherTok}[1]{\\\\textcolor[rgb]{0.00,0.44,0.13}{{#1}}}\\
\\\\newcommand{\\\\AlertTok}[1]{\\\\textcolor[rgb]{1.00,0.00,0.00}{\\\\textbf{{#1}}}}\\
\\\\newcommand{\\\\FunctionTok}[1]{\\\\textcolor[rgb]{0.02,0.16,0.49}{{#1}}}\\
\\\\newcommand{\\\\RegionMarkerTok}[1]{{#1}}\\
\\\\newcommand{\\\\ErrorTok}[1]{\\\\textcolor[rgb]{1.00,0.00,0.00}{\\\\textbf{{#1}}}}\\
\\\\newcommand{\\\\NormalTok}[1]{{#1}}\\
END

[ -n "$MDFRAMED" ] && RPL=$RPL$MRPL
if [ -n "$SHOUTPUT" ] ; then
    RPL=$RPL$DRPL
    LST_IF_PTN='^\$if(listings)\$$'
fi
[ -n "$HLCOMPAT" ] && RPL=$RPL$CRPL

pandoc -D latex |
sed -e "/$LST_IF_PTN/N;/$LST_IF2_PTN/,/$ENDIF_PTN/d" \
    -e "/$HL_M_PTN/i \\$RPL" \
    -e "/$HL_M_IF_PTN/,/$ENDIF_PTN/d" \
    -e "/$VERB_N_IF_PTN/N;/$VERB_N_IF2_PTN/,/$ENDIF_PTN/d"

