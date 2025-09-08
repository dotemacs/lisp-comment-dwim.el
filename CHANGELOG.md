# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 1.1.0

### Fixed

- Preserved formatting when (un)commenting. The formatting would be
lost, because `read` & `prin1` were used, so the s-expression would be
added as a single line.

## 1.0.0

Initial release
