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
;;
;; To use shellter, add the following to your Emacs configuration:
;;
;;   (require 'shellter)
;;   (shellter-mode 1)

;;; Code:

(require 'eshell)
(require 'perspective)

(defgroup shellter nil
  "Perspective-aware eshell session management."
  :group 'eshell
  :prefix "shellter-")

(defcustom shellter-default-session-name "main"
  "Default name for eshell sessions."
  :type 'string
  :group 'shellter)

(defcustom shellter-session-name-format "*eshell: %s [%s]*"
  "Format string for eshell buffer names.
First %s is the perspective name, second %s is the session name."
  :type 'string
  :group 'shellter)

(defvar shellter--sessions (make-hash-table :test 'equal)
  "Hash table mapping perspective names to their eshell sessions.")

;;;###autoload
(define-minor-mode shellter-mode
  "Toggle perspective-aware eshell session management."
  :global t
  :group 'shellter
  (if shellter-mode
      (message "Shellter mode enabled")
    (message "Shellter mode disabled")))

(defun shellter--perspective-name ()
  "Get the current perspective name."
  (if (bound-and-true-p persp-mode)
      (persp-current-name)
    "default"))

(defun shellter--make-buffer-name (session-name)
  "Generate buffer name for SESSION-NAME in current perspective."
  (format shellter-session-name-format
          (shellter--perspective-name)
          session-name))

(defun shellter-list-sessions ()
  "List all eshell sessions in the current perspective."
  (interactive)
  ;; Placeholder for future implementation
  (message "Session listing not yet implemented"))

;;;###autoload
(defun shellter-new-session (session-name)
  "Create a new named eshell session with SESSION-NAME."
  (interactive "sSession name: ")
  ;; Placeholder for future implementation
  (message "Creating new session: %s" session-name))

;;;###autoload
(defun shellter-switch-session ()
  "Switch to an existing eshell session in the current perspective."
  (interactive)
  ;; Placeholder for future implementation
  (message "Session switching not yet implemented"))

(provide 'shellter)
;;; shellter.el ends here