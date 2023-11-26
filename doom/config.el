;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Nikita Bloshchanevich"
      user-mail-address "nikblos@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(after! org-agenda
  (add-to-list 'org-agenda-files "~/Diary/ToDo.org"))
;; (defun +my/add-to-list-q (l item)
;;   (unless (member item l)
;;     (push item l))
;;   l)
;; (after! org
;;   (setq-default org-done-keywords (+my/add-to-list-q org-done-keywords "SKIP")))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;;* config
(setq company-idle-delay 0)
(setq eldoc-idle-delay 0)

;;* keybindings
(map! :g
      "C-s" #'save-buffer

      "M-(" #'sp-wrap-round
      :leader "se" #'iedit-mode)
(defun +my/vertico-home ()
  (interactive)
  (delete-minibuffer-contents)
  (insert "~/"))
(map! :map vertico-map :g "C-h" #'+my/vertico-home)

(defun +org/insert-date-bullet ()
  (interactive)
  (org-ctrl-c-ret)
  (org-insert-time-stamp (current-time) 'with-hour-minute nil nil " ")
  (evil-insert 1))

(after! org-element
  ;; Shut up when previewing LaTeX.
  ;;
  ;; This only has an effect on the org-mode element cache, but it doesn't make
  ;; a performance difference, and the warning was *very* annoying.
  (advice-add 'org-element-at-point :around
              (lambda (f &rest args)
                (cl-letf (((symbol-function 'display-warning) #'ignore))
                  (apply f args)))))

;; minted
(after! ox-latex
  (setq org-latex-compiler "xelatex")
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

;; (after! org
;;   (load-file (concat doom-private-dir "+latex-snippets.el"))
;;   (map! :g :map org-mode-map
;;         "C-S-SPC" #'+latex-snippets-expand-or-complete)
;;   (set-company-backend! 'org-mode #'+latex-snippets-company)
;;   (load-file (concat doom-private-dir "+drawing.el")))

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
