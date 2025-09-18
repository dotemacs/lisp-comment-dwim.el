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
     (setq-local indent-tabs-mode nil)
     (insert ,content)
     (goto-char ,cursor-pos)
     ,@body))

(defmacro lisp-comment-dwim-test-with-macro (macro-value content cursor-pos expected-result &rest additional-body)
  "Test with a specific comment macro setting."
  (declare (indent 4))
  `(let ((lisp-comment-dwim-comment-macro ,macro-value))
     (lisp-comment-dwim-test-with-buffer ,content ,cursor-pos
       (lisp-comment-dwim)
       (should (string= (buffer-string) ,expected-result))
       ,@additional-body)))

(ert-deftest lisp-comment-dwim-test-remove-cursor-on-hash-nil ()
  "Test removing #+nil when cursor is positioned on the # character."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on #
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-remove-cursor-on-hash-or ()
  "Test removing #+(or) when cursor is positioned on the # character."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on #
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-remove-cursor-on-hash-and ()
  "Test removing #-(and) when cursor is positioned on the # character."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on #
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

;; Test removing comment macros when cursor is on opening parenthesis
(ert-deftest lisp-comment-dwim-test-remove-cursor-on-paren-nil ()
  "Test removing #+nil when cursor is positioned on the opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      7  ; cursor on (
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-remove-cursor-on-paren-or ()
  "Test removing #+(or) when cursor is positioned on the opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      7  ; cursor on (
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-remove-cursor-on-paren-and ()
  "Test removing #-(and) when cursor is positioned on the opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"
      8  ; cursor on (
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

;; Test adding comment macros to regular s-expressions
(ert-deftest lisp-comment-dwim-test-add-nil-to-regular-sexp ()
  "Test adding #+nil to a regular s-expression."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on (
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-add-or-to-regular-sexp ()
  "Test adding #+(or) to a regular s-expression."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on (
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-add-and-to-regular-sexp ()
  "Test adding #-(and) to a regular s-expression."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor on (
      "#-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-multiple-prefixes-first-removal-nil ()
  "Test removing the first #+nil from multiple #+nil prefixes."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil #+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-multiple-prefixes-first-removal-or ()
  "Test removing the first #+(or) from multiple #+(or) prefixes."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) #+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-multiple-prefixes-first-removal-and ()
  "Test removing the first #-(and) from multiple #-(and) prefixes."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) #-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "#-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-single-prefix-removal-nil ()
  "Test removing single #+nil prefix."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-single-prefix-removal-or ()
  "Test removing single #+(or) prefix."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-single-prefix-removal-and ()
  "Test removing single #-(and) prefix."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) (ql:quickload '(:dexador :jonathan) :quiet t)"
      1  ; cursor at beginning
      "(ql:quickload '(:dexador :jonathan) :quiet t)"))

(ert-deftest lisp-comment-dwim-test-indented-comment-nil ()
  "Test handling #+nil with proper indentation."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "  #+nil (some-function arg1 arg2)"
      3  ; cursor on #
      "  (some-function arg1 arg2)"))

(ert-deftest lisp-comment-dwim-test-indented-comment-or ()
  "Test handling #+(or) with proper indentation."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "  #+(or) (some-function arg1 arg2)"
      3  ; cursor on #
      "  (some-function arg1 arg2)"))

(ert-deftest lisp-comment-dwim-test-indented-comment-and ()
  "Test handling #-(and) with proper indentation."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "  #-(and) (some-function arg1 arg2)"
      3  ; cursor on #
      "  (some-function arg1 arg2)"))

;; Test adding comment macros to indented s-expressions
(ert-deftest lisp-comment-dwim-test-add-nil-to-indented-sexp ()
  "Test adding #+nil to an indented s-expression."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "  (some-function arg1 arg2)"
      3  ; cursor on (
      "  #+nil (some-function arg1 arg2)"))

(ert-deftest lisp-comment-dwim-test-add-or-to-indented-sexp ()
  "Test adding #+(or) to an indented s-expression."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "  (some-function arg1 arg2)"
      3  ; cursor on (
      "  #+(or) (some-function arg1 arg2)"))

(ert-deftest lisp-comment-dwim-test-add-and-to-indented-sexp ()
  "Test adding #-(and) to an indented s-expression."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "  (some-function arg1 arg2)"
      3  ; cursor on (
      "  #-(and) (some-function arg1 arg2)"))

(ert-deftest lisp-comment-dwim-test-cursor-in-middle-of-line-nil ()
  "Test that cursor position anywhere on #+nil line removes the prefix."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil (defun test-func () 'test)"
      10  ; cursor somewhere in the middle
      "(defun test-func () 'test)"))

(ert-deftest lisp-comment-dwim-test-cursor-in-middle-of-line-or ()
  "Test that cursor position anywhere on #+(or) line removes the prefix."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) (defun test-func () 'test)"
      10  ; cursor somewhere in the middle
      "(defun test-func () 'test)"))

(ert-deftest lisp-comment-dwim-test-cursor-in-middle-of-line-and ()
  "Test that cursor position anywhere on #-(and) line removes the prefix."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) (defun test-func () 'test)"
      11  ; cursor somewhere in the middle
      "(defun test-func () 'test)"))

;; Test handling multiline s-expressions
(ert-deftest lisp-comment-dwim-test-multiline-sexp-nil ()
  "Test handling multiline s-expressions with #+nil."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(defun test-function (x)\n  (+ x 1))"
      1  ; cursor on first (
      "#+nil (defun test-function (x)\n        (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-multiline-sexp-or ()
  "Test handling multiline s-expressions with #+(or)."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(defun test-function (x)\n  (+ x 1))"
      1  ; cursor on first (
      "#+(or) (defun test-function (x)\n         (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-multiline-sexp-and ()
  "Test handling multiline s-expressions with #-(and)."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(defun test-function (x)\n  (+ x 1))"
      1  ; cursor on first (
      "#-(and) (defun test-function (x)\n          (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-remove-multiline-sexp-nil ()
  "Test removing #+nil from multiline s-expressions."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "#+nil (defun test-function (x)\n        (+ x 1))"
      1  ; cursor on #
      "(defun test-function (x)\n  (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-remove-multiline-sexp-or ()
  "Test removing #+(or) from multiline s-expressions."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "#+(or) (defun test-function (x)\n        (+ x 1))"
      1  ; cursor on #
      "(defun test-function (x)\n  (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-remove-multiline-sexp-and ()
  "Test removing #-(and) from multiline s-expressions."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "#-(and) (defun test-function (x)\n        (+ x 1))"
      1  ; cursor on #
      "(defun test-function (x)\n  (+ x 1))"))

(ert-deftest lisp-comment-dwim-test-region-remove-nil-comment ()
  "Test removing #+nil when selecting entire commented expression in region."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "#+nil (in-package :pantry)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(in-package :pantry)")))))

(ert-deftest lisp-comment-dwim-test-region-remove-or-comment ()
  "Test removing #+(or) when selecting entire commented expression in region."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "#+(or) (in-package :pantry)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(in-package :pantry)")))))

(ert-deftest lisp-comment-dwim-test-region-remove-and-comment ()
  "Test removing #-(and) when selecting entire commented expression in region."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "#-(and) (in-package :pantry)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(in-package :pantry)")))))

(ert-deftest lisp-comment-dwim-test-region-add-nil-comment ()
  "Test adding #+nil when selecting regular s-expression in region."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "(in-package :foo)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "#+nil (in-package :foo)")))))

(ert-deftest lisp-comment-dwim-test-region-add-or-comment ()
  "Test adding #+(or) when selecting regular s-expression in region."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "(in-package :foo)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "#+(or) (in-package :foo)")))))

(ert-deftest lisp-comment-dwim-test-region-add-and-comment ()
  "Test adding #-(and) when selecting regular s-expression in region."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "(in-package :foo)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "#-(and) (in-package :foo)")))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-add ()
  "Comment plain text with semicolon and wrap following sexp in reader macro."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "this is some text\n(foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "; this is some text\n#+nil (foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-add-or ()
  "Comment plain text with semicolon and wrap following sexp in #+(or)."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "this is some text\n(foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "; this is some text\n#+(or) (foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-add-and ()
  "Comment plain text with semicolon and wrap following sexp in #-(and)."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "this is some text\n(foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "; this is some text\n#-(and) (foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-remove ()
  "Remove semicolon and reader macro comments across the region."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "; this is some text\n#+nil (foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "this is some text\n(foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-remove-or ()
  "Remove semicolon and #+(or) reader macro comments across the region."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "; this is some text\n#+(or) (foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "this is some text\n(foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-lines-remove-and ()
  "Remove semicolon and #-(and) reader macro comments across the region."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "; this is some text\n#-(and) (foo :bar)\n"
        1
      (let ((start (point-min))
            (end (point-max)))
        (lisp-comment-dwim-region start end)
        (should (string=
                 (buffer-string)
                 "this is some text\n(foo :bar)\n"))))))

(ert-deftest lisp-comment-dwim-test-region-multiple-sexps-nil ()
  "Test region operation on multiple s-expressions with #+nil."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "(defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (let ((start-pos (region-beginning))
            (end-pos (region-end)))
        (lisp-comment-dwim-region start-pos end-pos)
        (should (string= (buffer-string) "#+nil (defun foo (x) x)\n#+nil (defun bar (y) y)"))))))

(ert-deftest lisp-comment-dwim-test-region-multiple-sexps-or ()
  "Test region operation on multiple s-expressions with #+(or)."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "(defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (let ((start-pos (region-beginning))
            (end-pos (region-end)))
        (lisp-comment-dwim-region start-pos end-pos)
        (should (string= (buffer-string) "#+(or) (defun foo (x) x)\n#+(or) (defun bar (y) y)"))))))

(ert-deftest lisp-comment-dwim-test-region-multiple-sexps-and ()
  "Test region operation on multiple s-expressions with #-(and)."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "(defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (let ((start-pos (region-beginning))
            (end-pos (region-end)))
        (lisp-comment-dwim-region start-pos end-pos)
        (should (string= (buffer-string) "#-(and) (defun foo (x) x)\n#-(and) (defun bar (y) y)"))))))

(ert-deftest lisp-comment-dwim-test-region-mixed-commented-uncommented-nil ()
  "Test region operation on mix of commented and uncommented s-expressions with #+nil."
  (let ((lisp-comment-dwim-comment-macro "#+nil"))
    (lisp-comment-dwim-test-with-buffer
        "#+nil (defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(defun foo (x) x)\n#+nil (defun bar (y) y)")))))

(ert-deftest lisp-comment-dwim-test-region-mixed-commented-uncommented-or ()
  "Test region operation on mix of commented and uncommented s-expressions with #+(or)."
  (let ((lisp-comment-dwim-comment-macro "#+(or)"))
    (lisp-comment-dwim-test-with-buffer
        "#+(or) (defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(defun foo (x) x)\n#+(or) (defun bar (y) y)")))))

(ert-deftest lisp-comment-dwim-test-region-mixed-commented-uncommented-and ()
  "Test region operation on mix of commented and uncommented s-expressions with #-(and)."
  (let ((lisp-comment-dwim-comment-macro "#-(and)"))
    (lisp-comment-dwim-test-with-buffer
        "#-(and) (defun foo (x) x)\n(defun bar (y) y)"
        1
      (set-mark (point))
      (goto-char (point-max))
      (lisp-comment-dwim-region (region-beginning) (region-end))
      (should (string= (buffer-string) "(defun foo (x) x)\n#-(and) (defun bar (y) y)")))))

(ert-deftest lisp-comment-dwim-test-cursor-on-nested-sexp-nil ()
  "Test commenting only nested s-expression when cursor is on its opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo (bar))"
      6  ; cursor on ( before bar
      "(foo #+nil (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-on-nested-sexp-or ()
  "Test commenting only nested s-expression when cursor is on its opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo (bar))"
      6  ; cursor on ( before bar
      "(foo #+(or) (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-on-nested-sexp-and ()
  "Test commenting only nested s-expression when cursor is on its opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo (bar))"
      6  ; cursor on ( before bar
      "(foo #-(and) (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-before-atom-in-nested-sexp-nil ()
  "Test commenting atom when cursor is positioned before it inside nested s-expression."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo bar baz)"
      6  ; cursor before 'bar'
      "(foo #+nil bar baz)"))

(ert-deftest lisp-comment-dwim-test-cursor-before-atom-in-nested-sexp-or ()
  "Test commenting atom when cursor is positioned before it inside nested s-expression."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo bar baz)"
      6  ; cursor before 'bar'
      "(foo #+(or) bar baz)"))

(ert-deftest lisp-comment-dwim-test-cursor-before-atom-in-nested-sexp-and ()
  "Test commenting atom when cursor is positioned before it inside nested s-expression."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo bar baz)"
      6  ; cursor before 'bar'
      "(foo #-(and) bar baz)"))

(ert-deftest lisp-comment-dwim-test-remove-comment-from-nested-sexp-nil ()
  "Test removing comment from nested s-expression when cursor is on comment macro."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo #+nil (bar))"
      6  ; cursor on # of #+nil
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-remove-comment-from-nested-sexp-or ()
  "Test removing comment from nested s-expression when cursor is on comment macro."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo #+(or) (bar))"
      6  ; cursor on # of #+(or)
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-remove-comment-from-nested-sexp-and ()
  "Test removing comment from nested s-expression when cursor is on comment macro."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo #-(and) (bar))"
      6  ; cursor on # of #-(and)
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-before-commented-sexp-nil ()
  "Test removing comment when cursor is positioned before the commented s-expression."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo #+nil (bar))"
      11  ; cursor right before (bar)
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-before-commented-sexp-or ()
  "Test removing comment when cursor is positioned before the commented s-expression."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo #+(or) (bar))"
      12  ; cursor right before (bar)
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-cursor-before-commented-sexp-and ()
  "Test removing comment when cursor is positioned before the commented s-expression."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo #-(and) (bar))"
      13  ; cursor right before (bar)
      "(foo (bar))"))

(ert-deftest lisp-comment-dwim-test-semicolon-comment-text ()
  "Test adding semicolon comment to regular text."
  (with-temp-buffer
    (insert "This is nice")
    (goto-char 1)  ; cursor on "T"
    (lisp-comment-dwim)
    (should (string= (buffer-string) "; This is nice"))))

(ert-deftest lisp-comment-dwim-test-semicolon-uncomment-text ()
  "Test removing semicolon comment from text."
  (with-temp-buffer
    (insert "; This is nice")
    (goto-char 1)  ; cursor on semicolon
    (lisp-comment-dwim)
    (should (string= (buffer-string) "This is nice"))))

(ert-deftest lisp-comment-dwim-test-semicolon-comment-indented-text ()
  "Test adding semicolon comment to indented text."
  (with-temp-buffer
    (insert "  Some text")
    (goto-char 3)  ; cursor on "S"
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  ; Some text"))))

(ert-deftest lisp-comment-dwim-test-semicolon-uncomment-indented-text ()
  "Test removing semicolon comment from indented text."
  (with-temp-buffer
    (insert "  ; Some text")
    (goto-char 3)  ; cursor on semicolon
    (lisp-comment-dwim)
    (should (string= (buffer-string) "  Some text"))))

(ert-deftest lisp-comment-dwim-test-mixed-comment-types ()
  "Test that text gets semicolon comments and s-expressions get reader macros."
  (with-temp-buffer
    (insert "Text line\n(foo bar)")
    (goto-char 1)
    (lisp-comment-dwim)
    (should (string= (buffer-string) "; Text line\n(foo bar)"))
    (goto-char 12)  ; cursor on "("
    (lisp-comment-dwim)
    (should (string= (buffer-string) "; Text line\n#+(or) (foo bar)"))))

(ert-deftest lisp-comment-dwim-test-empty-line-gets-semicolon ()
  "Test that empty lines get semicolon comments."
  (with-temp-buffer
    (insert "\n(foo bar)")
    (goto-char 1)  ; cursor on empty line
    (lisp-comment-dwim)
    (should (string= (buffer-string) "; \n(foo bar)"))))

(ert-deftest lisp-comment-dwim-test-multiline-sexp-cursor-on-opening-paren-nil ()
  "Test commenting multiline s-expression when cursor is on opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo\n #+nil (bar))"
      1  ; cursor on opening paren of (foo
      "#+nil (foo\n       #+nil (bar))"))

(ert-deftest lisp-comment-dwim-test-multiline-sexp-cursor-on-opening-paren-or ()
  "Test commenting multiline s-expression when cursor is on opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo\n #+(or) (bar))"
      1  ; cursor on opening paren of (foo
      "#+(or) (foo\n        #+(or) (bar))"))

(ert-deftest lisp-comment-dwim-test-multiline-sexp-cursor-on-opening-paren-and ()
  "Test commenting multiline s-expression when cursor is on opening parenthesis."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo\n #-(and) (bar))"
      1  ; cursor on opening paren of (foo
      "#-(and) (foo\n         #-(and) (bar))"))

(ert-deftest lisp-comment-dwim-test-simple-multiline-sexp-nil ()
  "Test commenting simple multiline s-expression."
  (lisp-comment-dwim-test-with-macro "#+nil"
      "(foo\n bar)"
      1  ; cursor on opening paren
      "#+nil (foo\n       bar)"))

(ert-deftest lisp-comment-dwim-test-simple-multiline-sexp-or ()
  "Test commenting simple multiline s-expression."
  (lisp-comment-dwim-test-with-macro "#+(or)"
      "(foo\n bar)"
      1  ; cursor on opening paren
      "#+(or) (foo\n        bar)"))

(ert-deftest lisp-comment-dwim-test-simple-multiline-sexp-and ()
  "Test commenting simple multiline s-expression."
  (lisp-comment-dwim-test-with-macro "#-(and)"
      "(foo\n bar)"
      1  ; cursor on opening paren
      "#-(and) (foo\n         bar)"))

(provide 'lisp-comment-dwim-test)

;;; lisp-comment-dwim-test.el ends here
