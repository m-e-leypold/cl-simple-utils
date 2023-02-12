# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

- Source code: [HEAD at gitlab.com][unreleased].
- Diff to previous release: [Compare with 1.0.0][unreleased/diff].

[unreleased]:      https://gitlab.com/m-e-leypold/cl-simple-utils/-/commits/main/
[unreleased/diff]: https://gitlab.com/m-e-leypold/cl-simple-utils/-/compare/1.0.0...main

### Added

- Makefile: Rules to push release tags.

### Fixed

- CHANGELOG: Typo in version 1.0.0.

## [1.0.0]

- Source code: [Tag 1.0.0][1.0.0]
- This is the first ever release.

[1.0.0]: https://gitlab.com/m-e-leypold/cl-simple-utils/-/tags/1.0.0


### Compatibility

The code base uses SBCL specific features, which have not been
bound/implemented for other lisps.


### Added

The following is basically the content of the release, since there was
no previous release against which we could specify a delta.


#### System "de.m-e-leypold.cl-simple-utils"

File: `simple-utils.lisp` --- Various useful utilities.

- `INJECT-PACKAGE-LOCAL-NICKNAME`: Add a package local nickname for a
  package P2 to a package P1 after P1 has been defined. Mostly useful
  when loading optional packages like tests or specifications.
- `HERE-TEXT`, `CONCATENATE-LINES`, `HERE-TEXT*` support visually
  formatted multi-line text literals. Various options for dedenting,
  indenting and prefixing are available.
- `DEFPACKAGE-DOC`: Defines a symbol as anchor for the package
  documentation.
- `DEFRESTART`: Document restarts and define convenience function to invoke.
- `SYMBOL-FULL-NAME`, `WITH-FULL-SYMBOL-NAMES`: Extract full symbol
  names (with package) and format full names when printing.
- `WITH-GENSYMS`: Generate symbols for macros with names that
  contain their placeholder names.


#### System "de.m-e-leypold.cl-simple-utils/wrapped-streams"

File: `wrapped-streams.lisp` --- Streams that indent text automatically.

- `BASIC-INDENTING-CHARACTER-OUTPUT-STREAM` --- Base class for a
  stream that adds whitespace in front of every line.
- `INDENTED-STREAM` --- create indented stream from given stream.
- `MAYBE-INDENTED-STREAM` --- Dito, but if feature not available, just return original stream.
- `WITH-OUTPUT-TO`, `WITH-ERROR-TO` --- Helpers to redirect
  `*STANDARD-OUTPUT*` and `*ERROR-OUTPUT*`.
- `WITH-INDENTED-OUTPUT`, `WITH-MAYBE-INDENTED-OUTPUT` --- Redirect
  `*STANDARD-OUTPUT*` and `*ERROR-OUTPUT*` to indenting streams.
- `WITH-CAPTURING-OUTPUT` --- Capture `*STANDARD-OUTPUT*` and
  `*ERROR-OUTPUT*` as string in variable.

This system uses Gray streams and is inherently not portable on Lisps
that do not support user defined streams of any kind. It is only
implemented for SBLC so even if user defined streams exist on a
different Lisp platform, explicit implementation of change of the
existing conditionals is required.


#### System de.m-e-leypold.cl-simple-utils/basic-test

File `basic-test.lisp` --- A simple regression test facility.

`DEFTEST!` defines tests, `RUN-TESTS!` runs them, `ASSERT!` is a
specific assertion for this facility. See package source for more and
for details.

Local test registries can be defined with `DEFTEST-REGISTRY!` and
`END-TEST-REGISTRY!`. They create a symbol that can be referenced in
other documentation strings and whose documentation string is a
generated overview of the registered tests


#### System "de.m-e-leypold.cl-simple-utils/tests"

Files `test.lisp` and `tests.lisp` --- The regression tests for this package.


#### More systems and files

- System "de.m-e-leypold.cl-simple-utils/load-all" or file `load.lisp`
  --- Loads all systems in the package.

- System "de.m-e-leypold.cl-simple-utils/prerequisites" --- Loads all
  dependencies of the packages. This is used by the _check-warn_ in
  the Makefile.

- Makefile --- Automates development from the shell.

  - `make check` will run the tests
  - `make check-warn` will check for warnings.
  - `Make check-all` will do both.

  The other targets are only useful for the package author: They
  configure git and publish changes back to all repositories.
