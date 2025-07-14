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

;;; Utility Functions

(defun shellter-switch-to-session (session)
  "Switch to SESSION's buffer using `display-buffer'."
  (when (shellter-session-live-p session)
    (switch-to-buffer-other-window (shellter-session-buffer session))))

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

           ;; Single session exists, switch to it
           ((= 1 (length sessions))
            (car sessions))

           ;; Multiple sessions, prompt for selection
           (t
            (let ((name (shellter-read-session sessions)))
              (shellter-get-or-create-session context name purpose))))))

    ;; Switch to the session
    (shellter-switch-to-session session)))

(provide 'shellter)

;;; shellter.el ends here
