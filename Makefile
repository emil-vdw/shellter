EMACS ?= emacs
EASK ?= eask

.PHONY: all
all: clean compile test

.PHONY: ci
ci: clean compile test lint

.PHONY: install
install:
	$(EASK) install-deps

.PHONY: compile
compile:
	$(EASK) compile

.PHONY: clean
clean:
	$(EASK) clean all

.PHONY: test
test:
	$(EASK) test ert test/*.el

.PHONY: lint
lint:
	-$(EASK) lint package
	-$(EASK) lint elisp-lint

.PHONY: checkdoc
checkdoc:
	$(EASK) lint checkdoc

.PHONY: fix
fix:
	find . -name "*.el" -not -path "./packages/*" -not -path "./.git/*" -exec $(EMACS) -batch -Q -l whitespace {} --eval "(setq-default indent-tabs-mode nil)" -f whitespace-cleanup -f save-buffer \;

.PHONY: package
package:
	$(EASK) package

.PHONY: install-eask
install-eask:
	npm install -g @emacs-eask/cli

.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all       - Clean, compile, and test"
	@echo "  ci        - Run full CI pipeline (clean, compile, test, lint)"
	@echo "  install   - Install package dependencies"
	@echo "  compile   - Byte-compile elisp files"
	@echo "  clean     - Remove compiled files"
	@echo "  test      - Run tests"
	@echo "  lint      - Run linters"
	@echo "  checkdoc  - Check documentation strings"
	@echo "  fix       - Run whitespace-cleanup on elisp files"
	@echo "  package   - Build package"
	@echo "  install-eask - Install Eask CLI tool"
