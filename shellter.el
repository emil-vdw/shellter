;;; shellter.el --- Perspective-aware eshell session management -*- lexical-binding: t -*-

;; Copyright (C) 2025 Emil van der Westhuizen

;; Author: Emil van der Westhuizen <emilvdw@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (perspective "2.8"))
;; Keywords: convenience, terminals, perspective
;; URL: https://github.com/emil-vdw/shellter

;;; Commentary:

;; Shellter provides perspective-aware eshell session management for Emacs.
;; It integrates with perspective.el to provide better eshell workflow when
;; working with multiple projects.
;;
;; Features:
;; - Perspective-aware eshell invocation
;; - Named eshell sessions within perspectives
;; - Easy switching between multiple eshell instances
;; - Configurable session selection with consult support
;; - Configurable buffer switching behaviour
;;
;; To use shellter, add the following to your Emacs configuration:
;;
;;   (require 'shellter)
;;   (shellter-mode 1)
;;
;; Session Selection:
;;
;; By default, shellter uses `completing-read' for session selection.
;; You can customize this behavior by setting `shellter-read-session-function':
;;
;;   ;; Use consult for enhanced session selection with preview
;;   (require 'shellter-consult)
;;   (setq shellter-read-session-function #'shellter-read-session-consult)
;;
;;   ;; Or provide your own function
;;   (setq shellter-read-session-function
;;         (lambda (sessions &optional prompt initial-input)
;;           ;; Your custom implementation here
;;           ;; Should return the selected session name
;;           ))
;;
;; The consult integration (available in shellter-consult.el) provides:
;; - Live preview of session buffers
;; - Better completion UI with vertical candidates
;;
;; Buffer Switching Behaviour:
;;
;; You can customize how shellter switches to session buffers:
;;
;;   ;; Always switch in other window (default)
;;   (setq shellter-switch-buffer-behaviour 'other-window)
;;
;;   ;; Always switch in same window
;;   (setq shellter-switch-buffer-behaviour 'same-window)
;;
;;   ;; Smart switching - same window if in shellter buffer, otherwise other window
;;   (setq shellter-switch-buffer-behaviour 'smart)

;;; Code:

(require 'cl-lib)
(require 'eshell)

(require 'shellter-context)


;;; Customization

(defgroup shellter nil
  "Shell terminal session management for Emacs."
  :group 'eshell
  :prefix "shellter-")

(defcustom shellter-read-session-function #'shellter-read-session-default
  "Function to use for reading session names.
The function should accept the following arguments:
  - SESSIONS: A list of `shellter-session' structs
  - PROMPT: Optional prompt string (defaults to \"Shellter session: \")
  - INITIAL-INPUT: Optional initial input

The function should return the selected session name as a string.
Non-matching input should be allowed to create new sessions.

Built-in options include:
  - `shellter-read-session-default': Uses `completing-read'

Additional options available:
  - `shellter-read-session-consult': Uses `consult--read' with preview
    (requires loading shellter-consult.el)

Users can provide custom functions following the same signature."
  :type 'function
  :group 'shellter)

(defcustom shellter-switch-buffer-behaviour 'other-window
  "Controls how shellter switches to session buffers.

Available options:
  - `other-window': Always switch in other window (default)
  - `same-window': Always switch in the same window
  - `smart': Switch in same window if current buffer is a shellter
    session, otherwise switch in other window"
  :type '(choice (const :tag "Other window" other-window)
                 (const :tag "Same window" same-window)
                 (const :tag "Smart" smart))
  :group 'shellter)

;;; Utility Functions

(defun shellter-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a shellter session buffer.
BUFFER defaults to the current buffer."
  (let ((buffer (or buffer (current-buffer)))
        (context (shellter-get-current-context)))
    (cl-some (lambda (session)
               (and (shellter-session-live-p session)
                    (eq buffer (shellter-session-buffer session))))
             (shellter-context-get-sessions context))))

(defun shellter-switch-to-session (session)
  "Switch to SESSION's buffer using the configured behaviour.
The behaviour is controlled by `shellter-switch-buffer-behaviour'."
  (when (shellter-session-live-p session)
    (let ((buffer (shellter-session-buffer session)))
      (pcase shellter-switch-buffer-behaviour
        ('same-window
         (switch-to-buffer buffer))
        ('other-window
         (switch-to-buffer-other-window buffer))
        ('smart
         (if (shellter-buffer-p)
             (switch-to-buffer buffer)
           (switch-to-buffer-other-window buffer)))))))

(defun shellter-read-session-default (sessions &optional prompt initial-input)
  "Read a session name from SESSIONS using `completing-read'.
PROMPT defaults to \"Shellter session: \".
INITIAL-INPUT is passed to `completing-read'.
Returns the selected name. Non-matching input is allowed."
  (let ((names (mapcar #'shellter-session-name sessions))
        (prompt (or prompt "Shellter session: ")))
    (completing-read prompt names nil nil initial-input)))

(defun shellter-read-session (sessions)
  "Read a session name from SESSIONS using configured function.
Returns the selected name. Non-matching input is allowed."
  (funcall shellter-read-session-function sessions))


(defun shellter-get-or-create-session (context name &optional purpose)
  "Get existing session by NAME or create new one in CONTEXT.
Optional PURPOSE is set on new sessions."
  (let ((session (shellter-context-find-session context name)))
    (unless session
      ;; Create new session with the given name, not auto-generated
      (setq session (shellter-create-session name))
      (when purpose
        (setf (shellter-session-purpose session) purpose))
      (shellter-context-add-session context session))
    session))

;;;###autoload
(defun shellter (&optional arg purpose)
  "Open or switch to a shellter session.
With prefix ARG, always create a new session.
Optional PURPOSE can be provided when called programmatically."
  (interactive "P")

  ;; Clean up dead sessions first
  (let* ((context (shellter-get-current-context))
         (sessions (shellter-context-get-sessions context))
         (session
          (cond
           ;; With prefix arg, always create new
           (arg
            (let ((name (shellter-generate-session-name)))
              (shellter-get-or-create-session context name purpose)))

           ;; No sessions exist, create one
           ((null sessions)
            (let ((name (shellter-generate-session-name)))
              (shellter-get-or-create-session context name purpose)))

           ;; Single session exists
           ((= 1 (length sessions))
            (let ((single-session (car sessions)))
              ;; Check if we're already in this session
              (if (and (shellter-session-live-p single-session)
                       (eq (current-buffer) (shellter-session-buffer single-session)))
                  (user-error "Already in the only shellter session")
                single-session)))

           ;; Multiple sessions, prompt for selection
           (t
            (let ((name (shellter-read-session sessions)))
              (shellter-get-or-create-session context name purpose))))))

    ;; Switch to the session if we have one
    (when session
      (shellter-switch-to-session session))))

(provide 'shellter)

;;; shellter.el ends here
