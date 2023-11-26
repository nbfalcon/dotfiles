;;; +drawing.el -*- lexical-binding: t; -*-

(defun +drawing--path-at-point ()
  (when-let ((link (org-element-lineage (org-element-context) '(link) t)))
    (let ((path (org-element-property :path link)))
      (when (or (string-suffix-p ".excalidraw.svg" path)
                (string-suffix-p ".excalidraw.png" path))
        (expand-file-name path)))))

(defun +drawing--open (path)
  (start-process "Excalidraw Webview" "*Excalidraw Webview*"
                 "python3" (expand-file-name "~/Projects/excalidraw-webview/src/excalidraw_webview.py") "--close-on-save" path))

(defvar +drawing--history '())

(defun +drawing/dwim ()
  (interactive)
  (if-let ((path (+drawing--path-at-point)))
      (+drawing--open path)
    (let* ((name (let ((name-or-empty (read-string "Name of drawing: " )))
                   (when (string-empty-p name-or-empty)
                     (user-error "Expected a name"))
                   name-or-empty))
           (name-l (downcase name))
           (name (if (or (string-suffix-p ".png" name-l)
                         (string-suffix-p ".svg" name-l)
                         (string-suffix-p ".excalidraw" name-l))
                     name
                   (concat name ".svg")))
           (as-path (if (string-match-p "\\`\\.?/" name) name (concat "./" name)))
           (ctx (org-element-at-point)))
      (if (and ctx (eq 'link (org-element-type ctx)))
          (goto-char (org-element-property :end ctx))
        (end-of-line)
        (when-let ((cb (char-before)))
          (unless (eq ?\s (char-syntax cb))
            (insert " "))))
      (insert (format "[[%s]]" as-path))
      (+drawing--open (expand-file-name as-path)))))
