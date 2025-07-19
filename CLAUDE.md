Shellter: Emacs Enhanced Eshell Session Manager

What it is: A monorepo containing Emacs packages for managing multiple named eshell sessions with perspective.el integration, solving the pain of juggling shells across projects.

# Monorepo Structure:
- packages/shellter: Core eshell session management
- packages/shellter-perspective: Perspective.el integration
- packages/shellter-consult: Consult integration for enhanced selection

# Core Architecture:
- Context System: Pluggable backends (global/perspective-aware) via shellter-context abstract class
- Naming Strategies: Configurable session naming (incremental/directory-based) via ellter-naming-strategy
- Session Management: shellter-session structs track name, buffer, and purpose with automatic cleanup

# Key Files:
- packages/shellter/shellter.el: Main interface and commands
- packages/shellter/shellter-context.el: Abstract context API
- packages/shellter/shellter-naming.el: Naming strategy framework
- packages/shellter-perspective/shellter-perspective.el: Perspective.el integration
- packages/shellter-consult/shellter-consult.el: Consult integration
- Eask: Workspace configuration
- packages/*/Eask: Individual package definitions

# Developer Quick Start:
- make install  # Install deps for all packages
- make test     # Run tests for all packages
- make lint     # Lint all packages
- make ci       # Full CI pipeline
- make test-shellter  # Test specific package
- make compile-shellter-perspective  # Compile specific package

# Key Features:
- Switch between named shells with completion
- Perspective-aware session isolation
- Automatic session naming based on directories
- Clean session management with dead buffer cleanup

# Individual Preferences
- @~/.claude/extra-context.md

Use concise, conventional commit style commits. Watch out for untracked files.
