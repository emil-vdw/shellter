# Shellter-Perspective

Perspective.el integration for Shellter.

## Overview

This package provides perspective-aware session management for Shellter, allowing you to:

- Maintain separate eshell sessions per perspective
- Automatically clean up sessions when perspectives are killed
- Share sessions between perspectives when needed

## Installation

```elisp
;; Install shellter first, then:
(straight-use-package
 '(shellter-perspective :type git :host github :repo "emil-vdw/shellter"
                        :files ("packages/shellter-perspective/*.el")))

(use-package shellter-perspective
  :straight (:type git :host github :repo "emil-vdw/shellter"
             :files ("packages/shellter-perspective/*.el"))
  :after (shellter perspective)
  :config
  (setq shellter-context-provider #'shellter-perspective-context-provider))
```

## Usage

Once configured, all shellter commands will operate within the current perspective:

- Each perspective maintains its own list of eshell sessions
- Switching perspectives shows only relevant sessions
- Sessions are automatically cleaned up when perspectives are deleted

## Configuration

```elisp
;; Enable perspective integration
(setq shellter-context-provider #'shellter-perspective-context-provider)

;; Optional: inherit global sessions in new perspectives
(setq shellter-perspective-inherit-global-sessions t)
```

## How It Works

The package implements a custom `shellter-context` that tracks which buffers belong to which perspectives. When you switch perspectives, only the sessions associated with the current perspective are visible through shellter commands.

## License

See the LICENSE file in the repository root.