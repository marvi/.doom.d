;;; history.el -*- lexical-binding: t; -*-

  (add-to-list 'org-latex-classes
               '("tufte-handout"
                 "\\documentclass{tufte-handout}
\\ifxetex
  \\newcommand{\\textls}[2][5]{%
    \\begingroup\\addfontfeatures{LetterSpace=#1}#2\\endgroup
  }
  \\renewcommand{\\allcapsspacing}[1]{\\textls[15]{#1}}
  \\renewcommand{\\smallcapsspacing}[1]{\\textls[10]{#1}}
  \\renewcommand{\\allcaps}[1]{\\textls[15]{\\MakeTextUppercase{#1}}}
  \\renewcommand{\\smallcaps}[1]{\\smallcapsspacing{\\scshape\\MakeTextLowercase{#1}}}
  \\renewcommand{\\textsc}[1]{\\smallcapsspacing{\\textsmallcaps{#1}}}
\\fi"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("blank"
                 "[NO-DEFAULT-PACKAGES]
               [NO-PACKAGES]
               [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("word" "\\documentclass{scrartcl}
                 \\usepackage{fontspec}
                 \\usepackage[svgnames]{xcolor}
                 \\defaultfontfeatures{Ligatures=TeX}
                 \\setsansfont{Calibri}
                 \\setmainfont{Cambria}
                 \\newfontfamily\\subsubsectionfont[Color=LightBlue]{TeX Gyre Termes}
                 \\addtokomafont{section}{\\color{DarkBlue}}
                 \\addtokomafont{subsection}{\\color{MediumBlue}}
                 \\addtokomafont{subsubsection}{\\normalfont\\itshape\\subsubsectionfont}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list 'org-latex-classes
  '("koma-article2" "\\documentclass[11pt,twopage,parskip=half-,headings=big,booktabs,longtable]{scrreprt}
                    \\usepackage[utf8]{inputenc}
                    \\usepackage{fontspec}
                    \\usepackage[svgnames, table]{xcolor}
                    \\usepackage[swedish]{babel}
                    \\defaultfontfeatures{Ligatures=TeX}
                    \\setsansfont{Soin Sans Pro}
                    \\setmainfont{Garvis Pro}
                    \\setmonofont{Jetbrains Mono}
                    \\addtokomafont{disposition}{\\normalfont\\sffamily}
                    \\usepackage{colortbl}
                    \\usepackage{siunitx}
                    \\usepackage{longtable}
                    \\usepackage{wrapfig}
                    \\usepackage{rotating}
                    \\usepackage[normalem]{ulem}
                    \\usepackage{amsmath}
                    \\usepackage{textcomp}
                    \\usepackage{amssymb}
                    \\usepackage{listings}
                    \\usepackage{xurl}
\\definecolor{listing-background}{HTML}{F7F7F7}
\\definecolor{listing-rule}{HTML}{B3B2B3}
\\definecolor{listing-numbers}{HTML}{B3B2B3}
\\definecolor{listing-text-color}{HTML}{000000}
\\definecolor{listing-keyword}{HTML}{435489}
\\definecolor{listing-keyword-2}{HTML}{1284CA} % additional keywords
\\definecolor{listing-keyword-3}{HTML}{9137CB} % additional keywords
\\definecolor{listing-identifier}{HTML}{435489}
\\definecolor{listing-string}{HTML}{00999A}
\\definecolor{listing-comment}{HTML}{8E8E8E}

\\lstdefinestyle{eisvogel_listing_style}{
  numbers          = left,
  xleftmargin      = 2.7em,
  framexleftmargin = 2.5em,
  backgroundcolor  = \\color{listing-background},
  basicstyle       = \\color{listing-text-color}\\small\\ttfamily{},
  breaklines       = true,
  frame            = single,
  framesep         = 0.19em,
  rulecolor        = \\color{listing-rule},
  frameround       = ffff,
  tabsize          = 4,
  numberstyle      = \\color{listing-numbers},
  aboveskip        = 1.0em,
  belowskip        = 0.1em,
  abovecaptionskip = 0em,
  belowcaptionskip = 1.0em,
  keywordstyle     = {\\color{listing-keyword}\\bfseries},
  keywordstyle     = {[2]\\color{listing-keyword-2}\\bfseries},
  keywordstyle     = {[3]\\color{listing-keyword-3}\\bfseries\\itshape},
  sensitive        = true,
  identifierstyle  = \\color{listing-identifier},
  commentstyle     = \\color{listing-comment},
  stringstyle      = \\color{listing-string},
  showstringspaces = false,
}
\\lstset{style=eisvogel_listing_style}"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("marvi"
                 "\\documentclass[twoside]{report}
                 \\usepackage[swedish]{babel}
                 \\usepackage{fontspec}
                 \\setmainfont{Calendas Plus}
                 \\setsansfont{Soin Sans Pro}[Scale=MatchLowercase]
                 \\setmonofont{Menlo}
                 \\usepackage{booktabs}
                 \\usepackage{subcaption}
                 \\usepackage[hypcap=true]{caption}
                 \\usepackage[table]{xcolor}
                 \\usepackage{colortbl}
                 \\usepackage{sectsty}
                 \\usepackage{siunitx}
                 \\allsectionsfont{\\sffamily}
                 \\usepackage{enumitem}
                 \\setlist[description]{style=unboxed}
                 \\usepackage{listings}

\\definecolor{listing-background}{HTML}{F7F7F7}
\\definecolor{listing-rule}{HTML}{B3B2B3}
\\definecolor{listing-numbers}{HTML}{B3B2B3}
\\definecolor{listing-text-color}{HTML}{000000}
\\definecolor{listing-keyword}{HTML}{435489}
\\definecolor{listing-keyword-2}{HTML}{1284CA} % additional keywords
\\definecolor{listing-keyword-3}{HTML}{9137CB} % additional keywords
\\definecolor{listing-identifier}{HTML}{435489}
\\definecolor{listing-string}{HTML}{00999A}
\\definecolor{listing-comment}{HTML}{8E8E8E}

\\lstdefinestyle{eisvogel_listing_style}{
  language         = java,
  numbers          = left,
  xleftmargin      = 2.7em,
  framexleftmargin = 2.5em,
  backgroundcolor  = \\color{listing-background},
  basicstyle       = \\color{listing-text-color}\\small\\ttfamily{},
  breaklines       = true,
  frame            = single,
  framesep         = 0.19em,
  rulecolor        = \\color{listing-rule},
  frameround       = ffff,
  tabsize          = 4,
  numberstyle      = \\color{listing-numbers},
  aboveskip        = 1.0em,
  belowskip        = 0.1em,
  abovecaptionskip = 0em,
  belowcaptionskip = 1.0em,
  keywordstyle     = {\\color{listing-keyword}\\bfseries},
  keywordstyle     = {[2]\\color{listing-keyword-2}\\bfseries},
  keywordstyle     = {[3]\\color{listing-keyword-3}\\bfseries\\itshape},
  sensitive        = true,
  identifierstyle  = \\color{listing-identifier},
  commentstyle     = \\color{listing-comment},
  stringstyle      = \\color{listing-string},
  showstringspaces = false,
  escapeinside     = {/*@}{@*/}, % Allow LaTeX inside these special comments
  literate         =
  {€}{{\\EUR}}1 {£}{{\\pounds}}1 {«}{{\\guillemotleft}}1
  {»}{{\\guillemotright}}1 {ñ}{{\\~n}}1 {Ñ}{{\\~N}}1 {¿}{{?`}}1
  {…}{{\\ldots}}1 {≥}{{>=}}1 {≤}{{<=}}1 {„}{{\\glqq}}1 {“}{{\\grqq}}1
  {”}{{''}}1
}
\\lstset{style=eisvogel_listing_style}
           \\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
                 \\usepackage{parskip}
                 \\makeatletter
                 \\renewcommand{\\maketitle}{%
                 \\begingroup\\parindent0pt
                 \\sffamily
                 \\Huge{\\bfseries\\@title}\\par\\bigskip
                 \\LARGE{\\bfseries\\@author}\\par\\medskip
                 \\normalsize\\@date\\par\\bigskip
                 \\endgroup\\@afterindentfalse\\@afterheading}
                 \\makeatother"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

