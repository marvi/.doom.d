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
                    "-activate" "oeg.gnu.Emacs"))
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


;; Good looks!

(custom-set-faces!
  '(outline-1 :weight semi-bold :height 1.6)
  '(outline-2 :weight bold :height 1.25)
  '(outline-3 :weight bold :height 1.0)
  '(outline-4 :weight semi-bold :height 1.0)
  '(outline-5 :weight semi-bold :height 1.0)
  '(outline-6 :weight semi-bold :height 100)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

;;; Org Export Stuff

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
