;;; lisp-comment-dwim.el --- Configurable comment reader macro toggle for Common Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Aleksandar Simic <a@repl.ist>
;; Version: 1.3.0
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

(defun lisp-comment-dwim--parse-sexp-at-point ()
  "Parse s-expression at point and return (START END FORM).
Returns nil if no valid s-expression found."
  (save-excursion
    (skip-chars-forward " \t\n\r")
    (when (not (eobp))
      (let ((start (point)))
        (condition-case nil
            (let ((form (read (current-buffer)))
                  (end (point)))
              (list start end form))
          (error nil))))))

;;;###autoload
(defun lisp-comment-dwim ()
  "Toggle comment reader macro for the next s-expression.
If the s-expression following the cursor starts with the configured comment macro, remove it.
Otherwise, add the comment macro at the beginning of the s-expression."
  (interactive)
  (save-excursion
    ;; First, check if we're anywhere within a commented expression
    (let* ((line-start (progn (beginning-of-line) (point)))
           (original-point (progn (end-of-line) (point)))
           (original-indent (lisp-comment-dwim--get-indentation line-start))
           (comment-regex (lisp-comment-dwim--get-comment-macro-regex))
           (comment-string (lisp-comment-dwim--get-comment-macro-string)))
      (goto-char line-start)
      (skip-chars-forward " \t")
      (cond
       ;; Case 1: Line starts with comment macro - remove one prefix
       ((looking-at comment-regex)
        (let ((start (point))
              (prefix-end (match-end 0)))
          ;; Skip any additional comment macro prefixes to find the actual form
          (goto-char prefix-end)
          (while (looking-at comment-regex)
            (goto-char (match-end 0)))
          (condition-case nil
              (let ((inner-form (read (current-buffer)))
                    (form-end (point)))
                ;; Remove just the first comment prefix
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
             ;; If we still can't parse, just remove the first comment prefix
             (delete-region start prefix-end)
             (lisp-comment-dwim--reindent-at-point original-indent)
             (message "Removed %s comment" lisp-comment-dwim-comment-macro)))))
       ;; Case 2: Regular s-expression - add comment prefix
       (t
        (skip-chars-forward " \t\n\r")
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
              (message "Added %s comment" lisp-comment-dwim-comment-macro)))))))))

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
        (comment-regex (lisp-comment-dwim--get-comment-macro-regex))
        (comment-string (lisp-comment-dwim--get-comment-macro-string)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not (eobp)))
        (skip-chars-forward " \t\n\r" end)
        (when (< (point) end)
          (let ((line-start (progn (beginning-of-line) (point)))
                (original-indent (lisp-comment-dwim--get-indentation (point))))
            (goto-char line-start)
            (skip-chars-forward " \t")
            (cond
             ;; Case 1: Looking at comment macro - remove it
             ((looking-at comment-regex)
              (let ((prefix-start (point))
                    (prefix-end (match-end 0)))
                (goto-char prefix-end)
                (condition-case nil
                    (let ((form (read (current-buffer)))
                          (form-end (point)))
                      (delete-region prefix-start prefix-end)
                      (setq end (- end (- prefix-end prefix-start)))
                      (setq modified-count (1+ modified-count))
                      (goto-char (- form-end (- prefix-end prefix-start))))
                  (error
                   (delete-region prefix-start prefix-end)
                   (setq end (- end (- prefix-end prefix-start)))
                   (setq modified-count (1+ modified-count))))))
             ;; Case 2: Regular s-expression - add comment macro
             (t
              (condition-case nil
                  (let ((sexp-start (point))
                        (form (read (current-buffer)))
                        (sexp-end (point))
                        (comment-length (length comment-string)))
                    (goto-char sexp-start)
                    (insert comment-string)
                    (setq end (+ end comment-length))
                    (setq modified-count (1+ modified-count))
                    (goto-char (+ sexp-end comment-length)))
                (error
                 (goto-char end))))))))
      (message "Toggled %s comment on %d s-expression%s"
               lisp-comment-dwim-comment-macro
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
