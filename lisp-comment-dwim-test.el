;;; lisp-comment-dwim-test.el --- Tests for lisp-comment-dwim -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Aleksandar Simic <a@repl.ist>
;; Keywords: lisp, common-lisp, comments, tests

;;; Commentary:
;; ERT tests for lisp-comment-dwim package functionality.

;;; Code:

(require 'ert)
(require 'lisp-comment-dwim)

(defmacro lisp-comment-dwim-test-with-buffer (content cursor-pos &rest body)
  "Create a temporary buffer with CONTENT, position cursor at CURSOR-POS, and execute BODY."
  (declare (indent 2))
  `(with-temp-buffer
     (lisp-mode)
     (insert ,content)
     (goto-char ,cursor-pos)
     ,@body))

(ert-deftest lisp-comment-dwim-test-remove-nil-cursor-on-hash ()
  "Test removing #+nil when cursor is positioned on the # character."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-remove-nil-cursor-on-paren ()
  "Test removing #+nil when cursor is positioned on the opening parenthesis."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      7  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-add-nil-to-regular-sexp ()
  "Test adding #+nil to a regular s-expression."
  (lisp-comment-dwim-test-with-buffer
      "(ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-multiple-nil-prefixes-first-removal ()
  "Test removing the first #+nil from multiple #+nil prefixes."
  (lisp-comment-dwim-test-with-buffer
      "#+nil #+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-multiple-nil-prefixes-second-removal ()
  "Test removing the remaining #+nil after first removal."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-indented-nil-comment ()
  "Test handling #+nil with proper indentation."
  (lisp-comment-dwim-test-with-buffer
      "  #+nil (some-function arg1 arg2)"
      3  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  (some-function arg1 arg2)"))))

(ert-deftest lisp-comment-dwim-test-add-to-indented-sexp ()
  "Test adding #+nil to an indented s-expression."
  (lisp-comment-dwim-test-with-buffer
      "  (some-function arg1 arg2)"
      3  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  #+nil (some-function arg1 arg2)"))))

(ert-deftest lisp-comment-dwim-test-cursor-in-middle-of-nil-line ()
  "Test that cursor position anywhere on #+nil line removes the prefix."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (defun test-func () 'test)"
      10  ; cursor somewhere in the middle
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(defun test-func () 'test)"))))

(ert-deftest lisp-comment-dwim-test-multiline-sexp ()
  "Test handling multiline s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "(defun test-function (x)\n  (+ x 1))"
      1  ; cursor on first (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+nil (defun test-function (x) (+ x 1))"))))

(ert-deftest lisp-comment-dwim-test-remove-multiline-sexp ()
  "Test removing #+nil from multiline s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (defun test-function (x)\n        (+ x 1))"
      1  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(defun test-function (x)\n(+ x 1))"))))

(ert-deftest lisp-comment-dwim-test-region-remove-nil-comment ()
  "Test removing #+nil when selecting entire commented expression in region."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (in-package :pantry)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "(in-package :pantry)"))))

(ert-deftest lisp-comment-dwim-test-region-add-nil-comment ()
  "Test adding #+nil when selecting regular s-expression in region."
  (lisp-comment-dwim-test-with-buffer
      "(in-package :foo)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "#+nil (in-package :foo)"))))

(ert-deftest lisp-comment-dwim-test-region-multiple-sexps ()
  "Test region operation on multiple s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "(defun foo (x) x)\n(defun bar (y) y)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (let ((start-pos (region-beginning))
          (end-pos (region-end)))
      (lisp-comment-dwim-region start-pos end-pos)
      (should (string= (buffer-string) "#+nil (defun foo (x) x)\n#+nil (defun bar (y) y)")))))

(ert-deftest lisp-comment-dwim-test-region-mixed-commented-uncommented ()
  "Test region operation on mix of commented and uncommented s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "#+nil (defun foo (x) x)\n(defun bar (y) y)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "(defun foo (x) x)\n#+nil (defun bar (y) y)"))))

(provide 'lisp-comment-dwim-test)

;;; lisp-comment-dwim-test.el ends here
