;;; +latex-complete.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'yasnippet)

(defcustom +latex-snippets-complete-words t
  "FIXME"
  :type 'boolean)

(defun +latex-snippets--vec (n)
  (let* ((n (string-to-number n))
         (inner (cl-loop for i from 1 to n collect (format "${%d:x_%d} \\\\\\\\" i i))))
    (concat "\\begin{pmatrix} "
            (string-join inner " ")
            " \\end{pmatrix}")))

(defvar +latex-snippets-regex-snippets
  '(("rv\\([[:digit:]]*\\)" +latex-snippets--vec :complete "roundvec")))
(defvar +latex-snippets-extra-canidates
  '("rv3"))
(defvar +latex-snippets-named-snippets
  '(("lim" "\\lim\\limits_{x \\to ${1:\\infty}}{${2:f(x)}}" :wrap 2)
    ("abs" "\\left|${1:abs}\\right|")
    ("sum" "\\sum_{i=$1}^{$2} ${3:i}")))
(defvar +latex-snippets-functions
  '(("frac" ("nom" "denom"))
    ("sqrt" ("rad"))
    ("text" ("text"))
    ("stackrel" ("above" "below"))))

(defun +latex-snippets--snippet-matcher (s) (elt s 0))
(defun +latex-snippets--snippet-expansion (s) (elt s 1))


;;* utils
(defun +latex-snippets--at-point ()
  (when-let ((word-bounds (bounds-of-thing-at-point 'symbol)))
    (let* ((start (car word-bounds))
           (end (cdr word-bounds))
           (word (buffer-substring-no-properties start end))
           (bs (cl-position ?\\ word :from-end t)))
      (when (or bs +latex-snippets-complete-words)
        (when bs (setq start (+ start bs)))
        (cons start end)))))

(defun +latex-snippets--find (word)
  (or (cl-find-if (lambda (s) (string-match (+latex-snippets--snippet-matcher s) word))
                  +latex-snippets-regex-snippets)
      (cl-find-if (lambda (s) (string= word (+latex-snippets--snippet-matcher s)))
                  +latex-snippets-named-snippets)
      (when-let ((f (cl-find-if (lambda (f) (string= word (+latex-snippets--snippet-matcher f)))
                                +latex-snippets-functions)))
        (let* ((args (+latex-snippets--snippet-expansion f))
               (args-yas (cl-loop for arg in args for i from 1
                                  collect (format "{${%d:%s}}" i arg))))
          (list word (format "\\\\%s%s" word (string-join args-yas "")))))
      (when-let ((entity (cl-find word +latex-snippets--org-pretty-entities :test #'string=)))
        (list entity (concat "\\" entity)))))

(defun +latex-snippets--all-matches (s)
  (cl-loop for (start end) on (cddr (match-data)) by #'cddr
           collect (substring s start end)))

(defun +latex-snippets--expand (snippet word)
  (let ((expansion (+latex-snippets--snippet-expansion snippet)))
    (cond ((stringp expansion) expansion)
          ((functionp expansion) (apply expansion (+latex-snippets--all-matches word)))
          (t (error "Invalid expansion: %s" expansion)))))

(defun +latex-snippets--plist-x-get (plist key)
  (while (and plist (not (eq key (car plist))))
    (setq plist (cdr plist)))
  (cadr plist))

(defun +latex-snippets--wrap-which (snippet)
  (or (+latex-snippets--plist-x-get snippet :wrap) 1))

(defun +latex-snippets--substitue (snippet-text with which)
  (replace-regexp-in-string (format "${%d\\(:[^}]*\\)}" which) (yas-escape-text with)
                            snippet-text
                            t t))

(defun +latex-snippets--yas-expand (snippet &optional start end)
  (yas-expand-snippet snippet start end)
  (when (bound-and-true-p evil-mode)
    ;; In evil-mode (+ org?), not enabling insert-mode at this point causes yas to insta-exit
    (evil-insert-state)))


;;* interactive
(defvar +latex-snippets--org-pretty-entities
  (cl-delete-duplicates
   (cl-loop for entry in (append org-entities-user org-entities)
            when (and (listp entry)
                      (string-match-p "\\`\\\\[[:alpha:]]+\\({}\\)?\\'" (nth 1 entry)))
            collect (cl-destructuring-bind (_ name _ _ _ _ pretty) entry
                      (propertize (string-remove-prefix "\\" name)
                                  'pretty pretty)))
   :test #'string=))

(defun +latex-snippets--candidates ()
  (append +latex-snippets-extra-canidates
          (mapcar #'+latex-snippets--snippet-matcher +latex-snippets-named-snippets)
          (mapcar #'+latex-snippets--snippet-matcher +latex-snippets-functions)
          +latex-snippets--org-pretty-entities))

(defun +latex-snippets--completions (prefix)
  (let ((cands (+latex-snippets--candidates))
        (prefix (string-remove-prefix "\\" prefix)))
    (if (string-empty-p prefix)
        cands
      (cl-remove-if-not (lambda (c) (string-prefix-p prefix c))
                        cands))))

(defun +latex-snippets--completing-read ()
  (completing-read "LaTeX: " (+latex-snippets--candidates)))

(defun +latex-snippets-expand (word &optional start end)
  (interactive (cons (+latex-snippets--completing-read)
                     (when (or (region-active-p)
                               ;; This may be a DOOM/org-mode bug; normal visual
                               ;; (small v) selection does not trigger
                               ;; region-active-p
                               (eq 'visual (bound-and-true-p evil-state)))
                       (list (region-beginning) (region-end)))))
  (let ((word (string-remove-prefix "\\" word)))
    (if-let ((snippet (+latex-snippets--find word))
             (expansion (+latex-snippets--expand snippet word)))
        (if start
            ;; wrap
            (let* ((region (buffer-substring start end))
                   (expansion (+latex-snippets--substitue
                               expansion region
                               (+latex-snippets--wrap-which snippet))))
              (+latex-snippets--yas-expand expansion start end))
          (+latex-snippets--yas-expand expansion start end))
      (user-error "Could not expand: %s" word))))

(defun +latex-snippets-expand-at-point ()
  (interactive)
  (save-match-data
    (if-let ((prefix (+latex-snippets--at-point)))
        (cl-destructuring-bind (start . end) prefix
          (+latex-snippets-expand (buffer-substring start end) start end))
      (user-error "Nothing to expand found here"))))

;; Redundant with expand: it wraps if region
;; (defun +latex-snippets-wrap (start end &optional word)
;;   ;; Don't ask for a snippet if region would error later anyway; that would be bad UX
;;   (interactive "r")
;;   (or word (setq word (+latex-snippets--completing-read)))
;;   (save-match-data
;;     (let ((region (buffer-substring start end))
;;           (snippet (+latex-snippets--find word)))
;;       (when-let ((expanded (+latex-snippets--expand snippet word)))
;;         (let ((final (+latex-snippets--substitue expanded region (+latex-snippets--wrap-which snippet))))
;;           (+latex-snippets--yas-expand final start end))))))

;;* completion
(defun +latex-snippets--prefix ()
  (when-let ((sym (bounds-of-thing-at-point 'symbol)))
    (let* ((prev (buffer-substring-no-properties (car sym) (point)))
           (bs (cl-position ?\\ prev :from-end t)))
      (substring prev bs))))

(defun +latex-snippets-company (mode &optional arg &rest _)
  (interactive (list 'interactive))
  (cl-case mode
    (interactive (company-begin-backend '+latex-snippets-company))
    (prefix (+latex-snippets--prefix))
    (candidates (+latex-snippets--completions company-prefix))
    (post-completion
     (delete-region (- (point) (length company-prefix) (length arg)) (point))
     (+latex-snippets-expand arg))
    (kind 'snippet)
    (annotation (get-text-property 0 'pretty arg))))

(defun +latex-snippets-expand-or-complete ()
  (interactive)
  (let* ((prefix (or (+latex-snippets--prefix)
                     (user-error "Nothing to expand")))
         (cands (+latex-snippets--completions prefix)))
    (if (= 1 (length cands))
        (progn
          (delete-region (- (point) (length prefix)) (point))
          (+latex-snippets-expand (car cands)))
      (+latex-snippets-company 'interactive))))
