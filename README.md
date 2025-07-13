# Shellter

Eshell session management for Emacs.

## Overview

Shellter enhances your Emacs workflow by providing better integration between eshell and perspective.el. It addresses common pain points when working with multiple eshell sessions across different projects:

- **Named sessions** - Create and manage multiple named eshell sessions within each perspective
- **Smart switching** - Easily switch between eshell sessions with descriptive names

## Installation

### Via MELPA (coming soon)

```elisp
(use-package shellter
  :ensure t
  :config
  (shellter-mode 1))
```

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/emil-vdw/shellter.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/shellter")
(require 'shellter)
(shellter-mode 1)
```

## Requirements

- Emacs 26.1 or higher
- [perspective.el](https://github.com/nex3/perspective-el) 2.8 or higher

## Usage

### Basic Commands

- `shellter-new-session` - Create a new named eshell session in the current perspective
- `shellter-switch-session` - Switch to an existing eshell session
- `shellter-list-sessions` - List all eshell sessions in the current perspective

### Configuration

```elisp
;; Customize the default session name
(setq shellter-default-session-name "main")

;; Customize the buffer naming format
(setq shellter-session-name-format "*eshell: %s [%s]*")
```

## Development

### Setup

1. Install [Eask](https://emacs-eask.github.io/):
```bash
npm install -g @emacs-eask/cli
```

2. Install dependencies:
```bash
make install
```

### Running Tests

```bash
make test
```

### Linting

```bash
make lint
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Acknowledgments

- [perspective.el](https://github.com/nex3/perspective-el) for the excellent workspace management
- The Emacs community for continuous inspiration
