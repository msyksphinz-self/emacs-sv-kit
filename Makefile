# sv-kit -- SystemVerilog parser, linter and formatter for Emacs
EMACS ?= emacs
BATCH  = $(EMACS) --batch -Q -L lisp

LISP  = lisp/sv-lexer.el lisp/sv-parser.el lisp/sv-lint.el \
        lisp/sv-format.el lisp/sv-width.el lisp/sv-index.el lisp/sv-ide.el \
        lisp/sv-kit.el lisp/sv-mode.el
TESTS = test/sv-kit-test.el
RTL  ?= $(shell find ../../common -name '*.sv' 2>/dev/null)

.PHONY: all check test compile lint format format-check clean help

all: check

## check: byte-compile and run the test suite
check: compile test

## test: run the ERT suite
test:
	$(BATCH) -L test -l sv-kit-test -f ert-run-tests-batch-and-exit

## compile: byte-compile every file, treating warnings as errors
compile: clean
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile $(LISP)

## lint: run the linter over the project RTL
lint:
	@test -n "$(RTL)" || { echo "no RTL found; set RTL=..."; exit 1; }
	./bin/sv-kit lint $(RTL)

## format-check: fail when some RTL file is not formatted
format-check:
	./bin/sv-kit format --check $(RTL)

## format: reformat the project RTL in place
format:
	./bin/sv-kit format --write $(RTL)

## clean: remove byte-compiled files
clean:
	rm -f lisp/*.elc test/*.elc

## help: list the targets
help:
	@grep -E '^## ' $(MAKEFILE_LIST) | sed 's/^## /  /'
