# `lisp-comment-dwim`

Toggle `#+(or)` reader macro comments for Common Lisp s-expressions in
Emacs.

## Overview

This package provides intelligent toggling of `#+(or)` comments in
Common Lisp code. Unlike traditional line-based commenting, `#+(or)` is
a reader macro that comments out entire s-expressions, making it ideal
for temporarily disabling Lisp forms while preserving code structure.

## Example usage

**Commenting out code:**
```lisp
;; Before (cursor anywhere on the line)
(ql:quickload '(:dexador :jonathan) :quiet t)

;; After running lisp-comment-dwim
#+(or) (ql:quickload '(:dexador :jonathan) :quiet t)
```

**Uncommenting code:**
```lisp
;; Before (cursor anywhere on the line)
#+(or) (defun my-function (x) (+ x 1))

;; After running lisp-comment-dwim
(defun my-function (x) (+ x 1))
```

## Installation

### Via use-package

```elisp
(use-package lisp-comment-dwim
  :vc (:fetcher github :repo dotemacs/lisp-comment-dwim.el)
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

- `M-x lisp-comment-dwim` - Toggle `#+(or)` comment for s-expression at
  cursor
- `M-x lisp-comment-dwim-region` - Toggle `#+(or)` comments for all
  s-expressions in region
- `M-x lisp-comment-dwim-toggle-dwim` - Intelligently toggle (region
  if active, otherwise single s-expression), useful if you have this
  function bound to a binding, so that it can operate on s-expressions
  or regions, with a single function.

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

## Why `#+(or)`?

The `#+(or)` reader macro is a Common Lisp feature that conditionally
reads code based on feature expressions. `#+(or)` effectively comments
out the following s-expression at read-time. This is superior to line
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
