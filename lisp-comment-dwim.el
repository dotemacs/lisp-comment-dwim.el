;;; lisp-comment-dwim.el --- Comment #+(or) toggle for Common Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Aleksandar Simic <a@repl.ist>
;; Version: 1.2.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: lisp, common-lisp, comments, tools
;; URL: https://github.com/dotemacs/lisp-comment-dwim.el

;;; Commentary:

;; This package provides `lisp-comment-dwim' which toggles #+(or)
;; reader macro comments for Common Lisp s-expressions.

;;; Code:

(require 'cl-lib)

(defgroup lisp-comment-dwim nil
  "Comment #+(or) toggling for Common Lisp."
  :group 'lisp
  :prefix "lisp-comment-dwim-")

(defcustom lisp-comment-dwim-whitespace-after-nil " "
  "Whitespace to insert after #+(or)."
  :type 'string
  :group 'lisp-comment-dwim)

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
  "Toggle #+(or) comment for the next s-expression.
If the s-expression following the cursor starts with #+(or), remove it.
Otherwise, add #+(or) at the beginning of the s-expression."
  (interactive)
  (save-excursion
    ;; First, check if we're anywhere within a #+(or) commented expression
    (let* ((line-start (progn (beginning-of-line) (point)))
           (original-point (progn (end-of-line) (point)))
           (original-indent (lisp-comment-dwim--get-indentation line-start)))
      (goto-char line-start)
      (skip-chars-forward " \t")
      (cond
       ;; Case 1: Line starts with #+(or) - remove one #+(or) prefix
       ((looking-at "#\\+(or)\\s-+")
        (let ((start (point))
              (prefix-end (match-end 0)))
          ;; Skip any additional #+(or) prefixes to find the actual form
          (goto-char prefix-end)
          (while (looking-at "#\\+(or)\\s-+")
            (goto-char (match-end 0)))
          (condition-case nil
              (let ((inner-form (read (current-buffer)))
                    (form-end (point)))
                ;; Remove just the first #+(or) prefix
                (delete-region start prefix-end)
                (let ((sexp-start start))
                  (goto-char sexp-start)
                  (condition-case nil
                      (indent-sexp)
                    (error nil))
                  (goto-char sexp-start)
                  (lisp-comment-dwim--reindent-at-point original-indent))
                (message "Removed #+(or) comment"))
            (error
             ;; If we still can't parse, just remove the first #+(or) prefix
             (delete-region start prefix-end)
             (lisp-comment-dwim--reindent-at-point original-indent)
             (message "Removed #+(or) comment")))))
       ;; Case 2: Regular s-expression - add #+(or) prefix
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
              (insert "#+(or) ")
              (lisp-comment-dwim--reindent-at-point original-indent)
              (message "Added #+(or) comment")))))))))

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
  "Toggle #+(or) comment for each s-expression in region."
  (interactive "r")
  (let ((modified-count 0))
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
             ;; Case 1: Looking at #+(or) - remove it
             ((looking-at "#\\+(or)\\s-+")
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
             ;; Case 2: Regular s-expression - add #+(or)
             (t
              (condition-case nil
                  (let ((sexp-start (point))
                        (form (read (current-buffer)))
                        (sexp-end (point))
                        (comment-length (length "#+(or) ")))
                    (goto-char sexp-start)
                    (insert "#+(or) ")
                    (setq end (+ end comment-length))
                    (setq modified-count (1+ modified-count))
                    (goto-char (+ sexp-end comment-length)))
                (error
                 (goto-char end))))))))
      (message "Toggled #+(or) comment on %d s-expression%s"
               modified-count
               (if (= modified-count 1) "" "s")))))


;;;###autoload
(defun lisp-comment-dwim-toggle-dwim ()
  "Intelligently toggle #+(or) comments.
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
