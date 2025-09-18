# `lisp-comment-dwim`

Toggle `#+nil`, `#+(or)` or `#-(and)` reader macro comments for Common
Lisp s-expressions in Emacs.

The comment macro option is configurable via:

```emacs-lisp
(setq lisp-comment-dwim-comment-macro "#+nil")
```

## Example usage

**Commenting out code:**
```lisp
;; Before
(ql:quickload '(:dexador :jonathan) :quiet t)

;; After running lisp-comment-dwim
#+nil (ql:quickload '(:dexador :jonathan) :quiet t)
```
**Uncommenting code:**
```lisp
;; Before
#+nil (defun my-function (x) (+ x 1))

;; After running lisp-comment-dwim
(defun my-function (x) (+ x 1))
```

If you happened to have a mix, of s-expressions & text:
```lisp
some comment
(foo bar)
```

`lisp-comment-dwim-region` will turn it into:

```lisp
; some comment
#+nil (foo bar)
```

Run it again, to revert it.

But if you have a mix, of commented & uncommented:

```lisp
; some comment
(foo bar)
```

`lisp-comment-dwim-region` will comment the uncommented and uncomment
the commented:

```lisp
some comment
#+nil (foo bar)
```

## Overview

This package provides intelligent toggling of comments in Common Lisp
code. Unlike traditional line-based commenting, all of the above
comments are a reader macro that comment out entire s-expressions,
making it ideal for temporarily disabling Lisp forms while preserving
code structure.

By default `#+(or)` is the comment. But if you'd like `#+nil` to be
used, specify it via:

```lisp
(setq lisp-comment-dwim-comment-macro "#+nil")
```

The **caveat** being that if you eval this in Common Lisp:

```lisp
(push :nil *features*)
```

the macro `#+nil` will **no longer be treated as a comment
macro**. That **does not apply** to `#+(or)` & `#-(and)` macros.


## Installation

### Via use-package

```elisp
(use-package lisp-comment-dwim
    :vc (:fetcher github :repo dotemacs/lisp-comment-dwim.el)
    :custom (lisp-comment-dwim-comment-macro "#+nil")
    :config (lisp-comment-dwim-setup-keybindings))
```

### Manual Installation

1. Download `lisp-comment-dwim.el`
2. Add it to your Emacs load path
3. Add to your init file:

```elisp
(require 'lisp-comment-dwim)
(lisp-comment-dwim-setup-keybindings)
```

## Usage

### Basic Commands

- `M-x lisp-comment-dwim` - Toggle reader macro comment for
  s-expression at cursor
- `M-x lisp-comment-dwim-region` - Toggle reader macro comments for
  all s-expressions and text in region
- `M-x lisp-comment-dwim-toggle-dwim` - Intelligently toggle (region
  if active, otherwise single s-expression), useful if you have this
  function bound to a binding, so that it can operate on s-expressions
  or text regions, with a single function.

### Key Bindings

Set up default key bindings for Common Lisp modes:

```elisp
(lisp-comment-dwim-setup-keybindings)
```

This binds `M-;` to `lisp-comment-dwim` in:
- `lisp-mode`
- `lisp-interaction-mode`
- `slime-repl-mode`
- `sly-mrepl-mode`
- `common-lisp-mode`


## Configuration

### Customization

```elisp
(setq lisp-comment-dwim-whitespace-after-nil " ") ; Default whitespace after #+(or)
```

### Custom Key Binding

```elisp
;; Bind to a different key
(define-key lisp-mode-map (kbd "C-c ;") #'lisp-comment-dwim)
```

## Why comment reader macros ?

The comment reader macro is a Common Lisp feature that conditionally
reads code based on feature expressions. It effectively comments out
the following s-expression at read-time. This is superior to line
comments (`;;`) because:

- Preserves s-expression structure
- Works with any s-expression regardless of formatting
- Can be easily toggled programmatically
- Maintains proper syntax highlighting

## Requirements

- Emacs 24.3+
- Common Lisp mode (built-in or external)

## Testing

Run the test suite:

```elisp
(load-file "lisp-comment-dwim.el")
(load-file "lisp-comment-dwim-test.el")
(ert-run-tests-batch-and-exit)
```

Or interactively: `M-x ert RET t RET`
