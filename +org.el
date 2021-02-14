;;; +orgmode.el -*- lexical-binding: t; -*-

;; Custom functions

(defun marvi/org-insert-timestamp ()
  "Insert active timestamp at POS."
  (interactive)
  (insert (format "<%s>" (format-time-string "%Y-%m-%d %H:%M:%S"))))

(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("j" . "marvis functions")
      :desc "Insert timestamp at POS" "t" #'marvi/org-insert-timestamp)


(defun marvi/org-insert-date ()
  "Insert current date at POS."
  (interactive)
  (insert (format "%s" (format-time-string "%Y-%m-%d"))))

(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("j" . "marvis functions")
      :desc "Insert date at POS" "d" #'marvi/org-insert-date)

;; Deft for searching notes
(after! deft
  (setq deft-directory "~/org"
        deft-extensions '("org")
        deft-recursive t))

;; terminal-notifier for Pomodoro
(after! org-pomodoro
  (when (executable-find "terminal-notifier")
    (defun notify-osx (title message)
      (call-process "terminal-notifier"
                    nil 0 nil
                    "-group" "Emacs"
                    "-title" title
                    "-sender" "org.gnu.Emacs"
                    "-message" message
                    "-activate" "org.gnu.Emacs"))
    (add-hook 'org-pomodoro-finished-hook
              (lambda ()
                (notify-osx "Pomodoro completed!" "Time for a break.")))
    (add-hook 'org-pomodoro-break-finished-hook
              (lambda ()
                (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))
    (add-hook 'org-pomodoro-long-break-finished-hook
              (lambda ()
                (notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))
    (add-hook 'org-pomodoro-killed-hook
              (lambda ()
                (notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))))

(use-package! aggressive-fill-paragraph-mode
  :hook org-mode)

;; Good looks!

(custom-set-faces!
  '(outline-1 :weight semi-bold :height 1.4)
  '(outline-2 :weight bold :height 1.25)
  '(outline-3 :weight bold :height 1.0)
  '(outline-4 :weight semi-bold :height 1.0)
  '(outline-5 :weight semi-bold :height 1.0)
  '(outline-6 :weight semi-bold :height 100)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

;;; Org Export Stuff

(after! ox-latex
  (setq org-latex-compiler "xelatex")
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))

  (add-to-list 'org-latex-classes
               '("nontech" "\\documentclass[11pt,twopage,parskip=half-,headings=big,booktabs,longtable]{scrartcl}
                    \\usepackage[utf8]{inputenc}
                    \\usepackage{fontspec}
                    \\usepackage[svgnames, table]{xcolor}
                    \\usepackage[swedish]{babel}
                    \\defaultfontfeatures{Ligatures=TeX}
                    \\setsansfont{Soin Sans Pro}
                    \\setmainfont{Garvis Pro}
                    \\addtokomafont{disposition}{\\normalfont\\sffamily}
                    \\usepackage{colortbl}
                    \\usepackage{longtable}
                    \\usepackage{wrapfig}
                    \\usepackage{rotating}
                    \\usepackage[normalem]{ulem}
                    \\usepackage{textcomp}
                    \\usepackage{xurl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("tech"
                 "\\documentclass[twoside]{article}
                 \\usepackage[swedish]{babel}
                 \\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
                 \\usepackage{fontspec}
                 \\usepackage{parskip}
                 \\setmainfont{IBM Plex Serif}
                 \\setsansfont{IBM Plex Sans}[Scale=MatchLowercase]
                 \\setmonofont{IBM Plex Mono}
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
                   \\lstset{style=eisvogel_listing_style}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-default-class "tech")

  (setq org-latex-tables-booktabs t)
  (setq org-latex-listings 'listings)
  (setq org-latex-hyperref-template "
\\colorlet{greenyblue}{blue!70!green}
\\colorlet{blueygreen}{blue!40!green}
\\providecolor{link}{named}{greenyblue}
\\providecolor{cite}{named}{blueygreen}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=,
  urlcolor=link,
  citecolor=cite\n}
\\urlstyle{same}\n"))

;;; Stuff for Hugo Site Generator
(use-package! ox-hugo
  :after ox)

(provide '+org)

 ;;; +org.el ends here
