# Shellter-Consult

Consult integration for Shellter.

## Overview

This package enhances Shellter's session selection with Consult's powerful completion interface, providing:

- Live preview of eshell sessions while selecting
- Rich completion interface with filtering
- Respects shellter's buffer switching behavior during preview

## Installation

```elisp
;; Install shellter first, then:
(straight-use-package
 '(shellter-consult :type git :host github :repo "emil-vdw/shellter"
                    :files ("packages/shellter-consult/*.el")))

(use-package shellter-consult
  :straight (:type git :host github :repo "emil-vdw/shellter"
             :files ("packages/shellter-consult/*.el"))
  :after (shellter consult)
  :config
  (setq shellter-read-session-function #'shellter-read-session-consult))
```

## Usage

Once configured, `shellter-switch` and other commands that prompt for session selection will use Consult's interface automatically. You can:

- Preview sessions by navigating through candidates
- Use Consult's advanced filtering capabilities
- Create new sessions by entering a non-matching name

## Features

- **Live Preview**: See session contents without switching
- **Smart Window Management**: Preview respects `shellter-switch-buffer-behaviour`
- **Seamless Integration**: Works with all shellter commands

## Configuration

```elisp
;; Enable consult integration
(setq shellter-read-session-function #'shellter-read-session-consult)
```

The preview behavior follows your configured `shellter-switch-buffer-behaviour`:
- `'same-window`: Preview replaces current buffer
- `'other-window`: Preview uses other window
- `'smart`: Uses same window if in shellter buffer, otherwise other window

## License

See the LICENSE file in the repository root.