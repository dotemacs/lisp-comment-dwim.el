;;; lisp-comment-dwim.el --- Configurable comment reader macro toggle for Common Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Aleksandar Simic <a@repl.ist>
;; Version: 1.5.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: lisp, common-lisp, comments, tools
;; URL: https://github.com/dotemacs/lisp-comment-dwim.el

;;; Commentary:

;; This package provides `lisp-comment-dwim' which toggles configurable
;; reader macro comments (#+nil, #+(or), or #-(and)) for Common Lisp s-expressions.

;;; Code:

(require 'cl-lib)

(defgroup lisp-comment-dwim nil
  "Configurable comment reader macro toggling for Common Lisp."
  :group 'lisp
  :prefix "lisp-comment-dwim-")

(defcustom lisp-comment-dwim-comment-macro "#+(or)"
  "Reader macro to use for commenting s-expressions.
Valid options:
- \"#+nil\" - Uses #+nil reader macro
- \"#+(or)\" - Uses #+(or) reader macro (default)
- \"#-(and)\" - Uses #-(and) reader macro"
  :type '(choice (const :tag "#+nil" "#+nil")
                 (const :tag "#+(or)" "#+(or)")
                 (const :tag "#-(and)" "#-(and)"))
  :group 'lisp-comment-dwim)

(defcustom lisp-comment-dwim-whitespace-after-nil " "
  "Whitespace to insert after the comment macro."
  :type 'string
  :group 'lisp-comment-dwim)

(defun lisp-comment-dwim--get-comment-macro-regex ()
  "Get regex pattern for the current comment macro."
  (concat (regexp-quote lisp-comment-dwim-comment-macro) "\\s-+"))

(defun lisp-comment-dwim--get-comment-macro-string ()
  "Get the comment macro string to insert."
  (concat lisp-comment-dwim-comment-macro lisp-comment-dwim-whitespace-after-nil))

(defun lisp-comment-dwim--line-only-whitespace-p ()
  "Check if current line contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[[:space:]]*$")))

(defun lisp-comment-dwim--line-starts-with-semicolon-p ()
  "Check if current line starts with semicolon comment."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at ";")))

(defun lisp-comment-dwim--line-starts-with-reader-macro-p ()
  "Check if current line starts with any reader macro comment."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (or (looking-at "#\\+nil\\s-+")
        (looking-at "#\\+(or)\\s-+")
        (looking-at "#-(and)\\s-+"))))

(defun lisp-comment-dwim--at-s-expression-p ()
  "Check if cursor is positioned at or near an s-expression."
  (and (not (eobp))
       (looking-at "(")))

(defun lisp-comment-dwim--inside-parentheses-p ()
  "Check if cursor is inside parentheses (nested within a larger s-expression)."
  (save-excursion
    (condition-case nil
        (let ((start-pos (point)))
          (up-list -1)  ; Move to opening paren of containing sexp
          (< (point) start-pos))  ; If we moved backward, we're inside parens
      (error nil))))

(defun lisp-comment-dwim--comment-line-with-semicolon ()
  "Comment current line with semicolon."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (insert "; ")
    (message "Added semicolon comment")))

(defun lisp-comment-dwim--uncomment-line-with-semicolon ()
  "Remove semicolon comment from current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (looking-at ";+ *")
      (delete-region (match-beginning 0) (match-end 0))
      (message "Removed semicolon comment"))))

(defun lisp-comment-dwim--parse-sexp-at-point ()
  "Parse s-expression at point and return (START END FORM).
Returns nil if no valid s-expression found."
  (save-excursion
    (skip-chars-forward " \t\n\r")
    (when (not (eobp))
      (let ((start (point)))
        (condition-case nil
            (progn
              (forward-sexp 1)
              (let ((end (point)))
                (let ((form (condition-case nil
                                (progn
                                  (goto-char start)
                                  (read (current-buffer)))
                              (error 'unparseable-form))))
                  (list start end form))))
          (error nil))))))

;;;###autoload
(defun lisp-comment-dwim ()
  "Toggle comments intelligently.
For s-expressions: use reader macro comments (#+nil, #+(or), etc.).
For regular text: use semicolon comments.
If line starts with semicolon comment, remove it.
Otherwise, add appropriate comment type based on content."
  (interactive)
  (let ((original-point (point)))
    (save-excursion
      (goto-char original-point)

      (when (and (looking-at "\n")
                 (not (bolp)))
        (forward-line 1))

      (cond
       ((lisp-comment-dwim--line-starts-with-semicolon-p)
        (lisp-comment-dwim--uncomment-line-with-semicolon))

       ((lisp-comment-dwim--line-starts-with-reader-macro-p)
        (lisp-comment-dwim--handle-reader-macro-comments))

       ((lisp-comment-dwim--line-only-whitespace-p)
        (lisp-comment-dwim--comment-line-with-semicolon))

       ((save-excursion
          (goto-char original-point)
          (lisp-comment-dwim--inside-parentheses-p))
        (lisp-comment-dwim--handle-nested-commenting))

       (t
        (skip-chars-forward " \t\n\r")
        (cond
         ((lisp-comment-dwim--line-starts-with-semicolon-p)
          (lisp-comment-dwim--uncomment-line-with-semicolon))

         ((lisp-comment-dwim--at-s-expression-p)
          (lisp-comment-dwim--handle-reader-macro-comments))

         (t
          (goto-char original-point)
          (lisp-comment-dwim--comment-line-with-semicolon))))))))

(defun lisp-comment-dwim--handle-reader-macro-comments ()
  "Handle reader macro commenting for s-expressions."
  (save-excursion
    (let* ((original-point (point))
           (line-start (line-beginning-position))
           (original-indent (lisp-comment-dwim--get-indentation line-start))
           (comment-regex (lisp-comment-dwim--get-comment-macro-regex))
           (comment-string (lisp-comment-dwim--get-comment-macro-string)))
      (goto-char line-start)
      (skip-chars-forward " \t")
      (cond
       ((looking-at comment-regex)
        (let ((start (point))
              (prefix-end (match-end 0)))
          (goto-char prefix-end)
          (while (looking-at comment-regex)
            (goto-char (match-end 0)))
          (condition-case nil
              (let ((inner-form (read (current-buffer)))
                    (form-end (point)))
                (delete-region start prefix-end)
                (let ((sexp-start start))
                  (goto-char sexp-start)
                  (condition-case nil
                      (indent-sexp)
                    (error nil))
                  (goto-char sexp-start)
                  (lisp-comment-dwim--reindent-at-point original-indent))
                (message "Removed %s comment" lisp-comment-dwim-comment-macro))
            (error
             (delete-region start prefix-end)
             (lisp-comment-dwim--reindent-at-point original-indent)
             (message "Removed %s comment" lisp-comment-dwim-comment-macro)))))
       (t
        (if (lisp-comment-dwim--inside-parentheses-p)
            (goto-char original-point)
          (skip-chars-forward " \t\n\r"))
        (let ((parsed (lisp-comment-dwim--parse-sexp-at-point)))
          (if (not parsed)
              (message "No s-expression found after cursor")
            (let* ((start (nth 0 parsed))
                   (end (nth 1 parsed))
                   (form (nth 2 parsed))
                   (original-text (buffer-substring start end)))
              (goto-char start)
              (insert comment-string)
              (lisp-comment-dwim--reindent-at-point original-indent)
              (condition-case nil
                  (indent-sexp)
                (error nil))
              (message "Added %s comment" lisp-comment-dwim-comment-macro)))))))))


(defun lisp-comment-dwim--handle-nested-commenting ()
  "Simplified nested commenting logic."
  (let ((comment-regex (lisp-comment-dwim--get-comment-macro-regex))
        (comment-string (lisp-comment-dwim--get-comment-macro-string)))
    (cond
     ((looking-at comment-regex)
      (delete-region (point) (match-end 0))
      (message "Removed %s comment" lisp-comment-dwim-comment-macro))

     ((save-excursion
        (beginning-of-line)
        (when (re-search-forward (regexp-quote lisp-comment-dwim-comment-macro) (line-end-position) t)
          (let ((macro-start (match-beginning 0))
                (macro-end (match-end 0)))
            (goto-char macro-end)
            (skip-chars-forward " \t")
            (let ((whitespace-end (point)))
              (delete-region macro-start whitespace-end)
              (message "Removed %s comment" lisp-comment-dwim-comment-macro)
              t)))))

     (t
      (let ((parsed (lisp-comment-dwim--parse-sexp-at-point)))
        (if parsed
            (progn
              (goto-char (nth 0 parsed))
              (insert comment-string)
              (message "Added %s comment" lisp-comment-dwim-comment-macro))
          (insert comment-string)
          (message "Added %s comment" lisp-comment-dwim-comment-macro)))))))

(defun lisp-comment-dwim--get-indentation (pos)
  "Get the indentation level at position POS."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun lisp-comment-dwim--reindent-at-point (target-indent)
  "Reindent the current line to TARGET-INDENT level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((current-indent (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to target-indent))))

;;;###autoload
(defun lisp-comment-dwim-region (start end)
  "Toggle comment reader macro for each s-expression in region."
  (interactive "r")
  (let ((modified-count 0)
        (end-marker (copy-marker end)))
    (set-marker-insertion-type end-marker t)
    (save-excursion
      (goto-char start)
      (while (< (point) end-marker)
        (let* ((line-start (line-beginning-position))
               (line-has-sexp
                (save-excursion
                  (goto-char line-start)
                  (skip-chars-forward " \t")
                  (eq (char-after) ?\()))
               (before-tick (buffer-modified-tick)))
          (goto-char line-start)
          (cond
           ((lisp-comment-dwim--line-only-whitespace-p)
            nil)

           ((lisp-comment-dwim--line-starts-with-semicolon-p)
            (lisp-comment-dwim--uncomment-line-with-semicolon))

           ((lisp-comment-dwim--line-starts-with-reader-macro-p)
            (lisp-comment-dwim--handle-reader-macro-comments))

           (line-has-sexp
            (lisp-comment-dwim--handle-reader-macro-comments))

           (t
            (lisp-comment-dwim--comment-line-with-semicolon)))
          (when (/= before-tick (buffer-modified-tick))
            (setq modified-count (1+ modified-count)))
          (forward-line 1)))
      (message "Toggled comments on %d line%s"
               modified-count
               (if (= modified-count 1) "" "s")))))


;;;###autoload
(defun lisp-comment-dwim-toggle-dwim ()
  "Intelligently toggle comment reader macros.
If region is active, operate on region. Otherwise operate on next s-expression."
  (interactive)
  (if (use-region-p)
      (lisp-comment-dwim-region (region-beginning) (region-end))
    (lisp-comment-dwim)))


;;;###autoload
(defun lisp-comment-dwim-setup-keybindings ()
  "Set up default keybindings for lisp-comment-dwim.
Binds M-; to `lisp-comment-dwim-toggle-dwim' in Lisp modes."
  (dolist (mode-hook '(lisp-mode-hook
                       lisp-interaction-mode-hook
                       slime-repl-mode-hook
                       sly-mrepl-mode-hook
                       common-lisp-mode-hook))
    (when (boundp mode-hook)
      (add-hook mode-hook
                (lambda ()
                  (local-set-key (kbd "M-;") #'lisp-comment-dwim-toggle-dwim))))))

(provide 'lisp-comment-dwim)

;;; lisp-comment-dwim.el ends here
