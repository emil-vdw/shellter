;;; shellter-context.el --- Generic context API for shellter -*- lexical-binding: t -*-

;;; Commentary:
;; This file defines the generic context API for shellter, allowing
;; users to plug in different context management implementations.
;; The default implementation provides a simple global context.

;;; Code:

(require 'eieio)

;; Declare function to suppress package-lint warning - we handle version compatibility dynamically
(declare-function project-root "project" (project))

;; Forward declare cleanup function from shellter.el
(declare-function shellter--cleanup-session-on-kill "shellter" ())

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

(defun shellter-create-session (name &optional buffer)
  "Create a new shellter session with NAME and optional BUFFER."
  (unless buffer
    (let ((buffer-name (format "*eshell:%s*" name)))
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (unless (eq major-mode 'eshell-mode)
          (eshell-mode)))))
  (let ((session (make-shellter-session :name name :buffer buffer)))
    ;; Set up buffer-local tracking and cleanup
    (with-current-buffer buffer
      (setq-local shellter--session session)
      (add-hook 'kill-buffer-hook #'shellter--cleanup-session-on-kill nil t))
    session))

(defun shellter-session-live-p (session)
  "Check if SESSION's buffer is still alive."
  (and (shellter-session-p session)
       (shellter-session-buffer session)
       (buffer-live-p (shellter-session-buffer session))))

;;; Abstract Context Interface
;;;
;;; Context managers are responsible for managing shellter sessions
;;; and cleaning up dead sessions.
(defclass shellter-context ()
  ()
  "Abstract base class for shellter context implementations.
This class defines the interface that all context implementations
must follow. Context implementations are responsible for managing
collections of eshell sessions."
  :abstract t)

(defun shellter-context-p (object)
  "Return non-nil if OBJECT is a shellter context."
  (cl-typep object 'shellter-context))

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
  (oset context sessions
        (cons session (oref context sessions))))

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


(provide 'shellter-context)
;;; shellter-context.el ends here
