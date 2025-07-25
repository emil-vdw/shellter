;;; shellter.el --- Enhanced eshell session management for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Emil van der Westhuizen

;; Author: Emil van der Westhuizen <emilvdw@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, terminals, eshell
;; URL: https://github.com/emil-vdw/shellter

;;; Commentary:

;; Shellter provides context-aware eshell session management for Emacs.
;; It integrates with perspective.el to provide better eshell workflow when
;; working with multiple projects.

;;; Code:

(require 'cl-lib)
(require 'eshell)

(require 'shellter-context)
(require 'shellter-naming)


;;; Customization

(defgroup shellter nil
  "Shell terminal session management for Emacs."
  :group 'eshell
  :prefix "shellter-")

(defcustom shellter-read-session-function #'shellter-read-session-default
  "Function to use for selecting a session.
The function should accept one argument:
  - SESSIONS: A list of `shellter-session' structs

The function should return the selected session.

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

;;; Minor Mode

;;;###autoload
(define-minor-mode shellter-mode
  "Enable shellter session management with automatic naming."
  :global t
  :group 'shellter
  (if shellter-mode
      (shellter-naming-setup-hooks)
    (shellter-naming-teardown-hooks)))

(defun shellter--ensure-mode ()
  "Ensure `shellter-mode' is active by enabling it if necessary.
This function is used internally to guard shellter commands and
automatically enables the mode rather than signaling an error."
  (unless shellter-mode
    (shellter-mode 1)))

;;; Utility Functions

(defvar-local shellter--session nil
  "The shellter session associated with this buffer.
This variable holds a `shellter-session' struct that tracks
the session's name, buffer reference, and optional purpose.")

(defun shellter-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a shellter session buffer.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (and (boundp 'shellter--session)
         shellter--session
         (shellter-session-p shellter--session))))

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
  "Read a session from SESSIONS using `completing-read'."
  (let* ((completion-table
          (mapcar (lambda (session)
                    (cons (shellter-session-name session) session))
                  sessions))
         (selected-name (completing-read "Shellter session: " completion-table))
         (selected-session (cdr (assoc selected-name completion-table))))
    selected-session))

(defun shellter-read-session (sessions)
  "Read a session from SESSIONS using configured function.
Returns the selected name. Non-matching input is allowed."
  (funcall shellter-read-session-function sessions))


(defun shellter-get-or-create-session (context name &optional purpose)
  "Get existing session by NAME or create new one in CONTEXT.
If a session with NAME exists in CONTEXT, return it.
Otherwise, create a new session and add it to CONTEXT.
Optional PURPOSE is set on newly created sessions.
Returns the `shellter-session' struct."
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
  (shellter--ensure-mode)

  ;; Clean up dead sessions first
  (let* ((context (shellter-get-current-context))
         (sessions (shellter-context-get-sessions context))
         (session
          (cond
           ;; With prefix arg, always create new
           (arg
            (let ((name (shellter-generate-session-name nil purpose)))
              (shellter-get-or-create-session context name purpose)))

           ;; No sessions exist, create one
           ((null sessions)
            (let ((name (shellter-generate-session-name nil purpose)))
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
            (shellter-read-session sessions)))))

    ;; Switch to the session if we have one
    (when session
      (shellter-switch-to-session session))))

(provide 'shellter)

;;; shellter.el ends here
