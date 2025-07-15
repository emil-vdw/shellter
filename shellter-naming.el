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

(cl-defgeneric shellter-generate-suggestions (strategy context)
  "Generate a list of name suggestions using STRATEGY and CONTEXT.
CONTEXT is a `shellter-naming-context' struct.
Returns a list of suggested names for interactive completion."
  ;; Default: just generate one suggestion
  (list (shellter-generate-name strategy context)))

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
         (name base-name)
         (counter 2))
    ;; Find first available name
    (while (member name existing)
      (setq name (format "%s<%d>" base-name counter))
      (cl-incf counter))
    name))

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
         (name (if include-parent
                   (let ((parent (file-name-nondirectory
                                 (directory-file-name
                                  (file-name-directory dir))))
                         (current (file-name-nondirectory
                                  (directory-file-name dir))))
                     (format "%s/%s" parent current))
                 (file-name-nondirectory (directory-file-name dir))))
         (existing (shellter-naming-context-existing-names context)))
    ;; Make unique if necessary
    (shellter--make-unique-name name existing)))

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
         (base-name (shellter-command-naming--format-name purpose command directory)))
    ;; Make unique if necessary
    (shellter--make-unique-name base-name existing)))

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
                 (unique-name (shellter--make-unique-name new-name existing)))
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
             (naming-context (shellter-get-current-naming-context)))
        (when-let ((new-name (shellter-update-name naming-strategy
                                                   shellter--session
                                                   naming-context)))
          (setf (shellter-session-name shellter--session) new-name)
          ;; Update the buffer name to match
          (with-current-buffer (shellter-session-buffer session)
            (rename-buffer (format "*eshell:%s*" new-name) t))))))

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

(defun shellter--make-unique-name (base-name existing-names)
  "Make BASE-NAME unique among EXISTING-NAMES by appending numbers."
  (let ((name base-name)
        (counter 2))
    (while (member name existing-names)
      (setq name (format "%s<%d>" base-name counter))
      (cl-incf counter))
    name))

(cl-defun shellter-create-naming-context (&key
                                          base-name
                                          directory
                                          project-root
                                          (existing-names nil)
                                          purpose
                                          (metadata nil))
  "Create a naming context with the given parameters.

BASE-NAME is a hint for the session name.
DIRECTORY is the current working directory.
PROJECT-ROOT is the root directory of the current project.
EXISTING-NAMES is a list of already taken session names.
PURPOSE describes the intended use of the session.
METADATA is an alist of additional key-value pairs."
  (make-shellter-naming-context
   :base-name base-name
   :directory directory
   :project-root project-root
   :existing-names existing-names
   :purpose purpose
   :metadata metadata))

(defun shellter-get-current-naming-context (&optional base-name purpose)
  "Get naming context based on current environment.
Optional BASE-NAME and PURPOSE override detected values."
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
     :purpose purpose)))

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
    (shellter--name-ensure-unique
     (shellter-generate-name strategy context)
     (shellter-naming-context-existing-names context))))

(defun shellter-suggest-session-names (&optional base-name purpose)
  "Get name suggestions from the current naming strategy.
Optional BASE-NAME provides a hint for the name.
Optional PURPOSE describes the session's intended use."
  (let* ((strategy (shellter-get-naming-strategy))
         (context (shellter-get-current-naming-context base-name purpose)))
    (shellter-generate-suggestions strategy context)))

(add-hook 'eshell-pre-command-hook #'shellter--naming-update-session-name)

(provide 'shellter-naming)
;;; shellter-naming.el ends here
