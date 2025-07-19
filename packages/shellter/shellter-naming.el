;;; shellter-naming.el --- Naming strategies for shellter -*- lexical-binding: t -*-

;;; Commentary:
;; This file provides the naming strategy framework for shellter,
;; allowing flexible session naming based on various contexts.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'eshell)
(require 'shellter-context)

;; Declare function to suppress package-lint warning - we handle version compatibility dynamically
(declare-function project-root "project" (project))

;;; Customization

(defcustom shellter-naming-strategy-provider #'shellter-simple-naming-provider
  "Function that provides the naming strategy for shellter.
This function should return an instance of a subclass of
`shellter-naming-strategy'. The default provides simple incremental naming.

To use different naming strategies, set this to one of the built-in
provider functions or a custom function that returns a strategy instance:

  ;; Use directory-based naming
  (setopt shellter-naming-strategy-provider #'shellter-directory-naming-provider)

  ;; Use command-based naming
  (setopt shellter-naming-strategy-provider #'shellter-command-naming-provider)

The function will be called with no arguments."
  :type 'function
  :group 'shellter)

;;; Naming Context

(cl-defstruct shellter-naming-context
  "Context information available to naming strategies."
  (base-name nil :type (or null string)
             :documentation "Base name hint provided by user or system.")
  (directory nil :type (or null string)
             :documentation "Current working directory.")
  (project-root nil :type (or null string)
                :documentation "Project root directory if in a project.")
  (existing-names nil :type list
                  :documentation "List of existing session names in context.")
  (purpose nil :type (or null string)
           :documentation "Purpose or type of session being created.")
  (current-name nil :type (or null string)
                :documentation "Current name of the session being renamed (nil for new sessions).")
  (metadata nil :type list
            :documentation "Additional key-value pairs for extensibility."))

;;; Abstract Naming Strategy Interface

(defclass shellter-naming-strategy ()
  ()
  "Abstract base class for shellter naming strategies.
This class defines the interface that all naming strategies must follow.
Naming strategies are responsible for generating appropriate names for
new eshell sessions based on various context clues."
  :abstract t)

;; Required methods for all naming strategies

(cl-defgeneric shellter-generate-name (strategy context)
  "Generate a session name using STRATEGY based on CONTEXT.
CONTEXT should be a `shellter-naming-context' struct.
This method must be implemented by all subclasses of `shellter-naming-strategy'.
It should return a string suitable for use as a session name.
It must generate a unique name that doesn't clash with an existing session name
 in the context")

(cl-defgeneric shellter-update-name (strategy session new-context)
  "Update SESSION's name based on NEW-CONTEXT using STRATEGY.
SESSION is a `shellter-session' struct.
NEW-CONTEXT is a `shellter-naming-context' struct with updated information.
Returns the new name, or nil if no update is needed."
  ;; Default: no automatic updates
  nil)

;;; Default Simple Naming Strategy

(defclass shellter-simple-naming-strategy (shellter-naming-strategy)
  ((prefix :initform "eshell"
           :type string
           :documentation "Prefix for generated names."))
  "Simple incremental naming strategy.
Generates names like 'eshell', 'eshell<2>', 'eshell<3>', etc.")

(cl-defmethod shellter-generate-name ((strategy shellter-simple-naming-strategy) context)
  "Generate a simple incremental name."
  (let* ((prefix (oref strategy prefix))
         (base-name (or (shellter-naming-context-base-name context) prefix))
         (existing (shellter-naming-context-existing-names context))
         (current-name (shellter-naming-context-current-name context)))
    ;; Use the utility function that handles current-name
    (shellter--make-unique-name base-name existing current-name)))

;;; Directory-Based Naming Strategy

(defclass shellter-directory-naming-strategy (shellter-naming-strategy)
  ((include-parent :initarg :include-parent
                   :initform nil
                   :type boolean
                   :documentation "Include parent directory in name."))
  "Naming strategy based on current directory.")

(cl-defmethod shellter-generate-name ((strategy shellter-directory-naming-strategy) context)
  "Generate name based on current directory."
  (let* ((dir (or (shellter-naming-context-directory context)
                  default-directory))
         (include-parent (oref strategy include-parent))
         (dir-name (if include-parent
                       (let ((parent (file-name-nondirectory
                                     (directory-file-name
                                      (file-name-directory dir))))
                             (current (file-name-nondirectory
                                      (directory-file-name dir))))
                         (format "%s/%s" parent current))
                     (file-name-nondirectory (directory-file-name dir))))
         (name (format "eshell@%s" dir-name))
         (existing (shellter-naming-context-existing-names context))
         (current-name (shellter-naming-context-current-name context)))
    ;; Make unique if necessary
    (shellter--make-unique-name name existing current-name)))

(cl-defmethod shellter-update-name ((strategy shellter-directory-naming-strategy)
                                    session new-context)
  "Update name when directory changes."
  (let ((new-name (shellter-generate-name strategy new-context))
        (current-name (shellter-session-name session)))
    (unless (string= new-name current-name)
      new-name)))

;;; Command-Based Naming Strategy

(defclass shellter-command-naming-strategy (shellter-naming-strategy)
  ((fallback-command :initform "eshell"
                     :type string
                     :documentation "Command name to use when no command history exists."))
  "Naming strategy based on last executed command and current directory.
Names follow the format: \"[<purpose>]: <last-command>@<current-dir>\" or
\"<last-command>@<current-dir>\" when no purpose is set.

To use this strategy with dynamic name updates:

  ;; Set the naming strategy provider
  (setopt shellter-naming-strategy-provider #'shellter-command-naming-provider)")

(defun shellter-command-naming--extract-command-name (command-object)
  "Extract a readable command name from COMMAND-OBJECT.
COMMAND-OBJECT is typically from `eshell-last-command-name' and has forms like:
- #<Lisp object>
- #<function eshell/ls>
- #<function eshell/cd>
- A plain string for external commands"
  (cond
   ;; Handle nil case first
   ((null command-object) "shell")
   ;; Check if it's a printed representation first (string that looks like #<function ...>)
   ((and (stringp command-object)
         (string-match "#<function eshell/\\([^>]+\\)>" command-object))
    (match-string 1 command-object))
   ;; If it's already a plain string (external command), use it directly
   ((stringp command-object)
    command-object)
   ;; If it's a symbol (like 'eshell/cd)
   ((symbolp command-object)
    (let ((name (symbol-name command-object)))
      ;; Extract command from eshell/command format
      (if (string-match "^eshell/\\(.+\\)$" name)
          (match-string 1 name)
        name)))
   ;; If it's a function
   ((functionp command-object)
    (shellter-command-naming--extract-command-name (if (symbolp command-object)
                                                      command-object
                                                      'shell)))
   ;; Default case
   (t "shell")))

(defun shellter-command-naming--get-relative-directory ()
  "Get current directory relative to home directory."
  (let ((home (expand-file-name "~"))
        (current (expand-file-name default-directory)))
    (if (string-prefix-p home current)
        (concat "~" (substring current (length home)))
      current)))

(defun shellter-command-naming--format-name (purpose command directory)
  "Format session name from PURPOSE, COMMAND, and DIRECTORY."
  (let ((base (format "%s@%s" command directory)))
    (if purpose
        (format "[%s]: %s" purpose base)
      base)))

(cl-defmethod shellter-generate-name ((strategy shellter-command-naming-strategy) context)
  "Generate name based on last command and current directory."
  (let* ((purpose (shellter-naming-context-purpose context))
         (context-dir (shellter-naming-context-directory context))
         (directory (if context-dir
                       (let ((home (expand-file-name "~"))
                             (expanded (expand-file-name context-dir)))
                         (if (string-prefix-p home expanded)
                             (concat "~" (substring expanded (length home)))
                           expanded))
                     (shellter-command-naming--get-relative-directory)))
         (command (oref strategy fallback-command))
         (existing (shellter-naming-context-existing-names context))
         (current-name (shellter-naming-context-current-name context))
         (base-name (shellter-command-naming--format-name purpose command directory)))
    ;; Make unique if necessary
    (shellter--make-unique-name base-name existing current-name)))

(cl-defmethod shellter-update-name ((strategy shellter-command-naming-strategy)
                                    session new-context)
  "Update session name based on last executed command."
  (when (shellter-session-live-p session)
    (let ((buffer (shellter-session-buffer session)))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode)
                   (boundp 'eshell-last-command-name)
                   eshell-last-command-name)
          (let* ((command (shellter-command-naming--extract-command-name
                          eshell-last-command-name))
                 (directory (shellter-command-naming--get-relative-directory))
                 (purpose (shellter-session-purpose session))
                 (new-name (shellter-command-naming--format-name purpose command directory))
                 (existing (shellter-naming-context-existing-names new-context))
                 (current-name (shellter-naming-context-current-name new-context))
                 (unique-name (shellter--make-unique-name new-name existing current-name)))
            (unless (string= unique-name (shellter-session-name session))
              unique-name)))))))

(defun shellter--naming-update-session-name (&optional session)
  "Update current shellter session name after command execution.
This function is meant to be added to `eshell-post-command-hook'."
  (if-let ((session (or session
                        ;; If session is not passed, infer from current buffer
                        (and (boundp 'shellter--session)
                             (shellter-session-p shellter--session)
                             shellter--session))))
      (let* ((context (shellter-get-current-context))
             (naming-strategy (shellter-get-naming-strategy))
             ;; Pass the current session name to the naming context
             (naming-context (shellter-get-current-naming-context
                              nil
                              nil
                              (shellter-session-name session))))
        (when-let ((new-name (shellter-update-name naming-strategy
                                                   session
                                                   naming-context)))
          (setf (shellter-session-name session) new-name)
          ;; Update the buffer name to match
          (with-current-buffer (shellter-session-buffer session)
            (rename-buffer (format "*%s*" new-name) t))))))

;;; Naming Strategy Providers

;; Singleton instances for built-in naming strategies
(defvar shellter--simple-naming-strategy-instance (make-instance 'shellter-simple-naming-strategy)
  "Singleton instance of the simple naming strategy.")

(defvar shellter--directory-naming-strategy-instance (make-instance 'shellter-directory-naming-strategy)
  "Singleton instance of the directory naming strategy.")

(defvar shellter--command-naming-strategy-instance (make-instance 'shellter-command-naming-strategy)
  "Singleton instance of the command naming strategy.")

(defun shellter-simple-naming-provider ()
  "Default naming strategy provider."
  shellter--simple-naming-strategy-instance)

(defun shellter-directory-naming-provider ()
  "Directory-based naming strategy provider."
  shellter--directory-naming-strategy-instance)

(defun shellter-command-naming-provider ()
  "Command-based naming strategy provider."
  shellter--command-naming-strategy-instance)

(defun shellter-get-naming-strategy ()
  "Get the current naming strategy based on the configured provider."
  (funcall shellter-naming-strategy-provider))

;;; Utility Functions

(defun shellter--make-unique-name (base-name existing-names &optional current-name)
  "Make BASE-NAME unique among EXISTING-NAMES by appending numbers.
If CURRENT-NAME is provided and BASE-NAME equals CURRENT-NAME, return it as-is."
  ;; If the base name is the same as the current name, no need to make it unique
  (if (and current-name (string= base-name current-name))
      base-name
    (let ((name base-name)
          (counter 2))
      (while (member name existing-names)
        (setq name (format "%s<%d>" base-name counter))
        (cl-incf counter))
      name)))

(cl-defun shellter-create-naming-context (&key
                                          base-name
                                          directory
                                          project-root
                                          (existing-names nil)
                                          purpose
                                          current-name
                                          (metadata nil))
  "Create a naming context with the given parameters.

BASE-NAME is a hint for the session name.
DIRECTORY is the current working directory.
PROJECT-ROOT is the root directory of the current project.
EXISTING-NAMES is a list of already taken session names.
PURPOSE describes the intended use of the session.
CURRENT-NAME is the current name when renaming a session.
METADATA is an alist of additional key-value pairs."
  (make-shellter-naming-context
   :base-name base-name
   :directory directory
   :project-root project-root
   :existing-names existing-names
   :purpose purpose
   :current-name current-name
   :metadata metadata))

(defun shellter-get-current-naming-context (&optional base-name purpose current-name)
  "Get naming context based on current environment.
Optional BASE-NAME and PURPOSE override detected values.
Optional CURRENT-NAME is the current session name being renamed."
  (let* ((context (shellter-get-current-context))
         (existing-names (mapcar #'shellter-session-name
                                (shellter-context-get-sessions context)))
         (project-root (when (fboundp 'project-current)
                        (when-let ((proj (project-current)))
                          ;; Handle different Emacs versions and project.el implementations
                          (cond
                           ;; Try calling project-root as a function if it exists
                           ((fboundp 'project-root)
                            (project-root proj))
                           ;; For vc projects in older Emacs versions, extract root from data structure
                           ((and (consp proj) (eq (car proj) 'vc))
                            ;; In Emacs 28 and earlier: (vc . root)
                            (cdr proj))
                           ;; For transient projects
                           ((and (consp proj) (eq (car proj) 'transient))
                            (cdr proj))
                           ;; Fallback: try to use vc-root-dir if available
                           ((fboundp 'vc-root-dir)
                            (ignore-errors (vc-root-dir default-directory)))
                           ;; Last resort: nil
                           (t nil))))))
    (shellter-create-naming-context
     :base-name base-name
     :directory default-directory
     :project-root project-root
     :existing-names existing-names
     :purpose purpose
     :current-name current-name)))

(defun shellter--name-ensure-unique (name existing-names)
  "Return a version of the shellter session NAME unique in EXISTING-NAMES."
  (let ((unique-name name)
        (counter 2))
    (while (member unique-name existing-names)
      (setq unique-name (format "%s<%d>" name counter))
      (cl-incf counter))
    unique-name))

;;; Integration Functions

(defun shellter-generate-session-name (&optional base-name purpose)
  "Generate a new session name using the current naming strategy.
Optional BASE-NAME provides a hint for the name.
Optional PURPOSE describes the session's intended use."
  (let* ((strategy (shellter-get-naming-strategy))
         (context (shellter-get-current-naming-context base-name purpose)))
    (shellter--make-unique-name
     (shellter-generate-name strategy context)
     (shellter-naming-context-existing-names context))))

(defun shellter-naming--on-session-cleanup (context session)
  "Update remaining session names after SESSION cleanup in CONTEXT.
This function is added to `shellter-session-cleanup-hook' to ensure
that session names are updated appropriately when a session is removed."
  (let* ((remaining-sessions (shellter-context-get-sessions context))
         (naming-context (shellter-get-current-naming-context)))
    (dolist (session remaining-sessions)
      (shellter--naming-update-session-name session))))

;;; Hook management

(defun shellter-naming-setup-hooks ()
  "Set up hooks for shellter naming functionality."
  (add-hook 'shellter-session-cleanup-hook #'shellter-naming--on-session-cleanup)
  (add-hook 'eshell-prepare-command-hook #'shellter--naming-update-session-name)
  ;; We need to also hook onto post command hook since the directory change
  ;; caused by "cd" commands only is not reflected `prepare-command-hook'.
  ;; In order to give naming strategy providers a chance to take into account
  ;; the current directory, we also use this hook.
  (add-hook 'eshell-post-command-hook #'shellter--naming-update-session-name))

(defun shellter-naming-teardown-hooks ()
  "Remove hooks for shellter naming functionality."
  (remove-hook 'shellter-session-cleanup-hook #'shellter-naming--on-session-cleanup)
  (remove-hook 'eshell-prepare-command-hook #'shellter--naming-update-session-name)
  (remove-hook 'eshell-post-command-hook #'shellter--naming-update-session-name))

(provide 'shellter-naming)
;;; shellter-naming.el ends here
