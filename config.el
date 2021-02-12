;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; First fix mac modifiers. I like to have Command behave like in Mac apps
;; Left Option I want as MacOS Option (to write accented characters, curly braces etc.)
;; Right Option should be Meta. Since I use vim mode I don't need Meta that often.
(setq mac-option-modifier nil
      mac-right-command-modifier 'meta)


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Markus Härnvi"
      user-mail-address "markus@harnvi.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;;
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Jetbrains Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "IBM Plex Serif" :size 15)
      doom-big-font (font-spec :family "Jetbrains Mono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(defun marvi/emacs-change-font ()
  "Change font based on available font list."
  (interactive)
  (let ((font (ivy-completing-read "font: " (font-family-list))))
    (setq doom-font (font-spec :family font :size 18)
          doom-big-font (font-spec :family font :size 24)))
  (doom/reload-font))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! graphviz-dot-mode
  :mode ("\\.dot\\'"))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)

(defun my/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (projectile-project-name))))

(setq tab-bar-mode t)
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*doom*")
(setq tab-bar-tab-name-function #'my/name-tab-by-project-or-default)

(after! doom-modeline
  (doom-modeline-def-segment workspace-name
  "The current workspace name or number.
Requires `eyebrowse-mode' or `tab-bar-mode' to be enabled."
  (when doom-modeline-workspace-name
    (when-let
        ((name (cond
                ((and (bound-and-true-p eyebrowse-mode)
                      (< 1 (length (eyebrowse--get 'window-configs))))
                 (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
                 (when-let*
                     ((num (eyebrowse--get 'current-slot))
                      (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                   (if (< 0 (length tag)) tag (int-to-string num))))
                (t
                 (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))))
      (propertize (format " %s " name) 'face
                  (if (doom-modeline--active)
                      'doom-modeline-buffer-major-mode
                    'mode-line-inactive))))))


(load! "+org")
(load! "+bindings")
