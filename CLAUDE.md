Shellter: Emacs Enhanced Eshell Session Manager

What it is: An Emacs package for managing multiple named eshell sessions with perspective.el integration, solving the pain of juggling shells across projects.

Core Architecture:
- Context System: Pluggable backends (global/perspective-aware) via shellter-context abstract class
- Naming Strategies: Configurable session naming (incremental/directory-based) via ellter-naming-strategy
- Session Management: shellter-session structs track name, buffer, and purpose with automatic cleanup

Key Files:
- shellter.el: Main interface and commands
- shellter-context.el: Abstract context API
- shellter-perspective.el: Perspective.el integration
- Eask: Package definition

Developer Quick Start:
- make install  # Install deps via Eask
- make test     # Run ERT tests
- make lint     # Lint with elisp-lint/package-lint
- make ci       # Full CI pipeline

Key Features:
- Switch between named shells with completion
- Perspective-aware session isolation
- Automatic session naming based on directories
- Clean session management with dead buffer cleanup
