;;; +orgmode.el -*- lexical-binding: t; -*-


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


(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))


(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}

\\usepackage[T1]{fontenc}
\\usepackage{mathtools}
\\usepackage{textcomp,amssymb}
\\usepackage[makeroom]{cancel}

\\usepackage{booktabs}

\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
% my custom stuff
\\usepackage{arev}
\\usepackage{arevmath}")

(provide '+org)

 ;;; +org.el ends here
