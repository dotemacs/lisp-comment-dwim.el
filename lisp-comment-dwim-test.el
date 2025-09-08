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
  "Test removing #+(or) when cursor is positioned on the # character."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-remove-nil-cursor-on-paren ()
  "Test removing #+(or) when cursor is positioned on the opening parenthesis."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      7  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-add-nil-to-regular-sexp ()
  "Test adding #+(or) to a regular s-expression."
  (lisp-comment-dwim-test-with-buffer
      "(ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-multiple-nil-prefixes-first-removal ()
  "Test removing the first #+(or) from multiple #+(or) prefixes."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) #+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-multiple-nil-prefixes-second-removal ()
  "Test removing the remaining #+(or) after first removal."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(ql:quickload '(:dexador :jonathan) :quiet t)"))))

(ert-deftest lisp-comment-dwim-test-indented-nil-comment ()
  "Test handling #+(or) with proper indentation."
  (lisp-comment-dwim-test-with-buffer
      "  #+(or) (some-function arg1 arg2)"
      3  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  (some-function arg1 arg2)"))))

(ert-deftest lisp-comment-dwim-test-add-to-indented-sexp ()
  "Test adding #+(or) to an indented s-expression."
  (lisp-comment-dwim-test-with-buffer
      "  (some-function arg1 arg2)"
      3  ; cursor on (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  #+(or) (some-function arg1 arg2)"))))

(ert-deftest lisp-comment-dwim-test-cursor-in-middle-of-nil-line ()
  "Test that cursor position anywhere on #+(or) line removes the prefix."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (defun test-func () 'test)"
      10  ; cursor somewhere in the middle
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(defun test-func () 'test)"))))

(ert-deftest lisp-comment-dwim-test-multiline-sexp ()
  "Test handling multiline s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "(defun test-function (x)\n  (+ x 1))"
      1  ; cursor on first (
    (lisp-comment-dwim)
    (should (string= (buffer-string) "#+(or) (defun test-function (x)\n  (+ x 1))"))))

(ert-deftest lisp-comment-dwim-test-remove-multiline-sexp ()
  "Test removing #+(or) from multiline s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (defun test-function (x)\n        (+ x 1))"
      1  ; cursor on #
    (lisp-comment-dwim)
    (should (string= (buffer-string) "(defun test-function (x)\n  (+ x 1))"))))

(ert-deftest lisp-comment-dwim-test-region-remove-nil-comment ()
  "Test removing #+(or) when selecting entire commented expression in region."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (in-package :pantry)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "(in-package :pantry)"))))

(ert-deftest lisp-comment-dwim-test-region-add-nil-comment ()
  "Test adding #+(or) when selecting regular s-expression in region."
  (lisp-comment-dwim-test-with-buffer
      "(in-package :foo)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "#+(or) (in-package :foo)"))))

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
      (should (string= (buffer-string) "#+(or) (defun foo (x) x)\n#+(or) (defun bar (y) y)")))))

(ert-deftest lisp-comment-dwim-test-region-mixed-commented-uncommented ()
  "Test region operation on mix of commented and uncommented s-expressions."
  (lisp-comment-dwim-test-with-buffer
      "#+(or) (defun foo (x) x)\n(defun bar (y) y)"
      1  ; start of selection
    (set-mark (point))
    (goto-char (point-max))  ; select entire buffer
    (lisp-comment-dwim-region (region-beginning) (region-end))
    (should (string= (buffer-string) "(defun foo (x) x)\n#+(or) (defun bar (y) y)"))))

(ert-deftest lisp-comment-dwim-test-preserves-multiline-formatting ()
  "Test that multiline formatting is preserved when commenting and uncommenting."
  (let ((original-multiline "(handler-case
    (load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))
  (error ()
    (format t \"Warning: Quicklisp not found. Make sure cl-markdown is installed.~%\")))"))
    (lisp-comment-dwim-test-with-buffer
        original-multiline
        1  ; cursor on first (
      (lisp-comment-dwim)
      (let ((commented-result (buffer-string)))
        (should (string-prefix-p "#+(or) " commented-result))
        (should (string-match-p "\n" commented-result))
        (goto-char 1)
        (lisp-comment-dwim)
        (should (string= (buffer-string) original-multiline))))))

(provide 'lisp-comment-dwim-test)

;;; lisp-comment-dwim-test.el ends here
