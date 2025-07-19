# Shellter

Enhanced eshell session management for Emacs.

## Overview

Shellter provides a flexible framework for managing multiple named eshell sessions in Emacs. It features:

- Named eshell sessions with intelligent naming strategies
- Pluggable context system for session organization
- Automatic cleanup of dead sessions
- Extensible architecture for integrations

## Installation

```elisp
;; Using straight.el
(straight-use-package
 '(shellter :type git :host github :repo "emil-vdw/shellter"
            :files ("packages/shellter/*.el")))

;; Or with use-package
(use-package shellter
  :straight (:type git :host github :repo "emil-vdw/shellter"
             :files ("packages/shellter/*.el")))
```

## Usage

Basic commands:

- `shellter-switch` - Switch to or create a named eshell session
- `shellter-rename-session` - Rename the current session

## Configuration

```elisp
;; Choose a naming strategy
(setq shellter-naming-strategy 'shellter-incremental-naming-strategy) ; default
;; or
(setq shellter-naming-strategy 'shellter-directory-naming-strategy)

;; Customize buffer switching behavior
(setq shellter-switch-buffer-behaviour 'smart) ; 'same-window, 'other-window, or 'smart
```

## Extending Shellter

Shellter provides two main extension points:

1. **Context Providers**: Implement `shellter-context` to define how sessions are organized
2. **Naming Strategies**: Implement `shellter-naming-strategy` to customize session naming

See the shellter-perspective and shellter-consult packages for examples.

## License

See the LICENSE file in the repository root.