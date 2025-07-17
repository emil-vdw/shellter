EMACS ?= emacs
EASK ?= eask

# Package directories
PACKAGES := shellter shellter-perspective shellter-consult
PACKAGE_DIRS := $(addprefix packages/,$(PACKAGES))

.PHONY: all
all: clean compile test

.PHONY: ci
ci: clean compile test lint

.PHONY: install
install:
	@echo "Installing dependencies for all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Installing $$pkg dependencies..."; \
		(cd $$pkg && $(EASK) install-deps) || exit 1; \
	done

.PHONY: compile
compile:
	@echo "Compiling all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Compiling $$pkg..."; \
		(cd $$pkg && $(EASK) compile) || exit 1; \
	done

.PHONY: clean
clean:
	@echo "Cleaning all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Cleaning $$pkg..."; \
		(cd $$pkg && $(EASK) clean all) || exit 1; \
	done

.PHONY: test
test:
	@echo "Testing all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Testing $$pkg..."; \
		if [ -d "$$pkg/test" ] && [ -n "$$(ls -A $$pkg/test/*.el 2>/dev/null)" ]; then \
			(cd $$pkg && $(EASK) test ert test/*.el) || exit 1; \
		else \
			echo "No tests found for $$pkg"; \
		fi; \
	done

.PHONY: lint
lint:
	@echo "Linting all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Linting $$pkg..."; \
		(cd $$pkg && $(EASK) lint package && $(EASK) lint elisp-lint) || true; \
	done

.PHONY: checkdoc
checkdoc:
	@echo "Checking documentation in all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Checking docs in $$pkg..."; \
		(cd $$pkg && $(EASK) lint checkdoc) || true; \
	done

.PHONY: fix
fix:
	find packages -name "*.el" -not -path "./.git/*" -exec $(EMACS) -batch -Q -l whitespace {} --eval "(setq-default indent-tabs-mode nil)" -f whitespace-cleanup -f save-buffer \;

.PHONY: package
package:
	@echo "Building all packages..."
	@for pkg in $(PACKAGE_DIRS); do \
		echo "Building $$pkg package..."; \
		(cd $$pkg && $(EASK) package) || exit 1; \
	done

# Individual package targets
.PHONY: $(addprefix compile-,$(PACKAGES))
$(addprefix compile-,$(PACKAGES)): compile-%:
	@echo "Compiling $*..."
	@cd packages/$* && $(EASK) compile

.PHONY: $(addprefix test-,$(PACKAGES))
$(addprefix test-,$(PACKAGES)): test-%:
	@echo "Testing $*..."
	@cd packages/$* && $(EASK) test ert test/*.el

.PHONY: $(addprefix clean-,$(PACKAGES))
$(addprefix clean-,$(PACKAGES)): clean-%:
	@echo "Cleaning $*..."
	@cd packages/$* && $(EASK) clean all

.PHONY: install-eask
install-eask:
	npm install -g @emacs-eask/cli

.PHONY: help
help:
	@echo "Available targets:"
	@echo ""
	@echo "Workspace targets (all packages):"
	@echo "  all       - Clean, compile, and test all packages"
	@echo "  ci        - Run full CI pipeline (clean, compile, test, lint)"
	@echo "  install   - Install dependencies for all packages"
	@echo "  compile   - Byte-compile all packages"
	@echo "  clean     - Remove compiled files from all packages"
	@echo "  test      - Run tests for all packages"
	@echo "  lint      - Run linters on all packages"
	@echo "  checkdoc  - Check documentation strings in all packages"
	@echo "  fix       - Run whitespace-cleanup on elisp files"
	@echo "  package   - Build all packages"
	@echo ""
	@echo "Individual package targets:"
	@echo "  compile-<pkg>  - Compile specific package (e.g., compile-shellter)"
	@echo "  test-<pkg>     - Test specific package (e.g., test-shellter)"
	@echo "  clean-<pkg>    - Clean specific package (e.g., clean-shellter)"
	@echo ""
	@echo "Available packages: $(PACKAGES)"
	@echo ""
	@echo "Utilities:"
	@echo "  install-eask - Install Eask CLI tool"
