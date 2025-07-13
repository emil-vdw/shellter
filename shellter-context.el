;;; shellter-context.el --- Generic context API for shellter -*- lexical-binding: t -*-

;;; Commentary:
;; This file defines the generic context API for shellter, allowing
;; users to plug in different context management implementations.
;; The default implementation provides a simple global context.

;;; Code:

(require 'eieio)

(defcustom shellter-context-provider #'shellter-global-context-provider
  "Function that provides the current context for shellter.
This function should return an instance of a subclass of `shellter-context'.
The default value provides a simple global context that is shared across
all Emacs sessions.

The function will be called with no arguments and should always return
a valid context object."
  :type 'function
  :group 'shellter)

;;; Session Management Structure

(cl-defstruct shellter-session
  "Structure representing an eshell session."
  (name nil :type string
        :documentation "The name of this eshell session.")
  (buffer nil :type (or null buffer)
          :documentation "The eshell buffer object associated with this session.")
  (purpose nil :type (or null string)
           :documentation "The purpose associated with this session, eg. python repl."))

;;; Abstract Context Interface

(defclass shellter-context ()
  ()
  "Abstract base class for shellter context implementations.
This class defines the interface that all context implementations
must follow. Context implementations are responsible for managing
collections of eshell sessions."
  :abstract t)

;; Generic functions that all context implementations must provide

(cl-defgeneric shellter-context-get-sessions (context)
  "Return all eshell session structs associated with CONTEXT.
This method must be implemented by all subclasses of `shellter-context'.
It should return a list of `shellter-session' structs.")

(cl-defgeneric shellter-context-add-session (context session)
  "Add SESSION to CONTEXT.
SESSION should be a `shellter-session' struct.
This method must be implemented by all subclasses of `shellter-context'.")

(cl-defgeneric shellter-context-remove-session (context session)
  "Remove SESSION from CONTEXT.
SESSION should be a `shellter-session' struct.
This method must be implemented by all subclasses of `shellter-context'.")

;; Optional generic functions with default implementations

(cl-defgeneric shellter-context-find-session (context name)
  "Find a session by NAME in CONTEXT.
Returns the `shellter-session' struct if found, nil otherwise."
  (cl-find name (shellter-context-get-sessions context)
           :key #'shellter-session-name
           :test #'string=))

(cl-defgeneric shellter-context-session-exists-p (context name)
  "Check if a session with NAME exists in CONTEXT."
  (not (null (shellter-context-find-session context name))))

(defun shellter-get-current-context ()
  "Get the current context based on the configured provider."
  (funcall shellter-context-provider))

;;; Default Global Context Implementation

(defclass shellter-global-context (shellter-context)
  ((sessions :initform nil
             :type list
             :documentation "List of sessions in this context."))
  "A simple global context that stores all sessions in a single list.
This is the default context implementation when no other context
system (like perspective.el) is available.")

(cl-defmethod shellter-context-get-sessions ((context shellter-global-context))
  "Return all sessions in the global context."
  (oref context sessions))

(cl-defmethod shellter-context-add-session ((context shellter-global-context) session)
  "Add SESSION to the global context."
  (unless (shellter-context-session-exists-p context (shellter-session-name session))
    (oset context sessions
          (cons session (oref context sessions)))))

(cl-defmethod shellter-context-remove-session ((context shellter-global-context) session)
  "Remove SESSION from the global context."
  (oset context sessions
        (cl-remove session (oref context sessions)
                   :test #'equal)))

(defvar shellter--global-context-instance (make-instance 'shellter-global-context)
  "The singleton instance of the global context.")

(defun shellter-global-context-provider ()
  "Default context provider that returns the global context instance."
  shellter--global-context-instance)

;;; Abstract naming style interface

(defcustom shellter-naming-strategy-provider #'shellter-simple-naming-provider
  "Function that provides the naming strategy for shellter.
This function should return an instance of a subclass of
`shellter-naming-strategy'. The default provides simple incremental naming.

To use different naming strategies, set this to a function that returns
your preferred strategy instance. For example:

  (setq shellter-naming-strategy-provider
        (lambda () (make-instance 'shellter-directory-naming-strategy)))

The function will be called with no arguments."
  :type 'function
  :group 'shellter)

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
  ((include-parent :initform nil
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

;;; Naming Strategy Provider Framework

(defun shellter-simple-naming-provider ()
  "Default naming strategy provider."
  (make-instance 'shellter-simple-naming-strategy))

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
    (mapcar (lambda (name)
              (shellter-validate-name strategy name context))
            (shellter-generate-suggestions strategy context))))

(provide 'shellter-context)
;;; shellter-context.el ends here
