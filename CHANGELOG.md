# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.4.0

### Added

- `lisp-comment-dwim` now (un)comments text as well as s-expressions

### Changed

- `lisp-comment-dwim` comments sub s-expressions, not just the first
  s-expression on the current line where the point is

## 1.3.0

### Changed

- All three comment reader macros can now be used for commenting:
  `#+nil`, `#+(or)` & `#-(and)`. `#+(or)` is default. To override it
  set it via `lisp-comment-dwim-comment-macro`.

## 1.2.0

### Changed

- Using `#+(or)` instead of `#+nil` for comments
- Docstrings & comments updated

### Removed

- Removed unused functions

## 1.1.0

### Fixed

- Preserved formatting when (un)commenting. The formatting would be
lost, because `read` & `prin1` were used, so the s-expression would be
added as a single line.

## 1.0.0

Initial release
