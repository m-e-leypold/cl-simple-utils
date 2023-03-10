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

- `DEFINE-CONSTANT` --- The macro from the SBCL manual to avoid
  problems when defining constants whose values when assigned
  repeatedly (compile and load time) are not EQ. See the SBCL manual
  and DEFCONST for details.
  
- `WITH-DOWNCASE-SYMBOLS`, `DOWNCASE-SYMBOL-NAME` --- Macros to obtain
  symbol names in lower case. Useful in case where we want to display
  a message to the use and display a symbol name that acts as a tag of
  some kind, but we do not want to display in upper case).

- simple-utils.lisp, tests.lisp: Facilities to overwrite
  `CL:DOCUMENTATION` in a way as to enable information collected at
  runtime be retrieved and display with `slime-decribe-symbol`
  
  - `DEFINE-DOCUMENTATION-NODE` --- Define a symbol where
    `DOCUMENTATION` calls (after going via `GET-DOCSTRING` (also a new
    function) the method `MAKE-DOCSTRING` (als added) on the item
    stored in the symbol. `BASE-DOCUMENTATION-NODE` is a suitable base
    type for such an item.
	
  - `DEFINE-DOCUMENTATION-ANCHOR` --- A more primitive way to define a
    symbol as a "documentation anchor" where `DOCUMENTATION` will just
    recur to a function passed to `DEFINE-DOCUMENTATION-ANCHOR`.
	
  - `MAKE-SYMBOL-INTO-DOCUMENTATION-ANCHOR`,
    `ADD-DOCUMENTATION-NODE-TO-FUNCTION`, `GET-DOCUMENTATION-NODE` ---
    Defining existing symbols a documentation anchors. This is geared
    towards functions with dynamically generated documentation strings.
	
	
- `PACKAGE-FROM-INDICATOR`, `WITH-PACKAGE-FROM-INDICATOR` --- Obtain
  package from an indicator that might be NIL, a symbol or a package
  itself. Useful when having a symbol at hand, but need to operate on
  the package or `*PACKAGE*` as default.

- simple-utils.lisp, tests.lisp: Facilities to execute hooks at end of
  file loading (e.g. in order to finalize processing of data that has
  been started with a declaration at the beginning of the file).

  - `ENSURE-LOAD-FILE-HOOKS` -- Itempotent. Ensure that load file
    hooks are installed during loading of this file. Used by macros
    which use this facility. See package
    [CL-SPECIFICATION][cl-specification] for an example.

  - `ADD-LOAD-FILE-HOOK` -- Add a hook to be processed when loading of
    this file has finished.

  - `END-OF-LOAD-FILE` -- Directive to be inserted at end of
    file. Starts "end of load" processing, i.e. processes all hooks
    that were added previously.
	
  - `GET-LOAD-FILE-HOOKS` -- Internally used to get at the load
    file hooks.
	
  - `LOAD-FILE-HOOKS-EXIST-P` --- Check if load file hooks have been
    installed. Useful as an indicator if the whole file is loaded or
    only single forms evaluated.
	
  - `GUESS-LOAD-SCOPE`, `FILE-LOAD-SCOPE-P` --- Check if a load of the
    whole file is in progress, or if only single forms are evaluated.
	
- simple-utils.lisp, tests.lisp: Facilities to maintain symbol during
  file load time in which information can collected during load
  time. This is useful for implementing DSLs that are not living in a
  single lisp form.

  - `CREATE-LOAD-TRACKER`, `REMOVE-LOAD-TRACKER` --- Creating and
    removing a load tracker. Actually trackers remove themselves when
    `END-OF-LOAD-FILE` is processed.
  
  - `GET-LOAD-TRACKER`, `WITH-LOAD-TRACKER`,
    `WITH-LOAD-TRACKER-VALUE`, `GET-LOAD-TRACKER-VALUE` --- Retrieving
    a tracker symbol or it's value and doing things with it.

[cl-specification]: https://gitlab.com/m-e-leypold/cl-specification

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
