;;; shellter-consult.el --- Consult integration for shellter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Emil van der Westhuizen

;; Author: Emil van der Westhuizen <emilvdw@gmail.com>
;; Package-Requires: ((emacs "26.1") (shellter "0.1.0") (consult "0.35"))
;; Keywords: convenience, terminals, perspective
;; URL: https://github.com/emil-vdw/shellter

;;; Commentary:

;; This package provides consult integration for shellter, enhancing
;; the session selection experience with live preview of session buffers.
;;
;; To use this integration, load this file and set:
;;
;;   (require 'shellter-consult)
;;   (setq shellter-read-session-function #'shellter-read-session-consult)
;;
;; This will enable consult's enhanced completion UI with preview
;; when selecting shellter sessions.

;;; Code:

(require 'shellter)
(require 'consult)
(require 'cl-lib)

;;; Consult integration

(defun shellter-read-session-consult (sessions &optional prompt initial-input)
  "Read a session name from SESSIONS using `consult--read'.
PROMPT defaults to \"Shellter session: \".
INITIAL-INPUT is passed to `consult--read'.
Returns the selected name.  Non-matching input is allowed.

This function provides enhanced features when consult is available:
- Live preview of session buffers"
  (let* ((names (mapcar #'shellter-session-name sessions))
         (prompt (or prompt "Shellter session: "))
         ;; Create state function for preview
         (state (let ((orig-buf (current-buffer))
                      (orig-win (selected-window)))
                  (lambda (action cand)
                    (pcase action
                      ('preview
                       ;; Preview the session buffer
                       (when cand
                         (when-let* ((session (cl-find cand sessions
                                                       :key #'shellter-session-name
                                                       :test #'string=))
                                     (buffer (shellter-session-buffer session)))
                           (when (buffer-live-p buffer)
                             (consult--buffer-action buffer)))))
                      ('return
                       ;; Restore original state
                       (when (window-live-p orig-win)
                         (select-window orig-win))
                       (when (buffer-live-p orig-buf)
                         (set-buffer orig-buf))))))))
    (consult--read names
                   :prompt prompt
                   :category 'shellter-session
                   :require-match nil
                   :initial initial-input
                   :state state
                   :preview-key 'any
                   :sort nil)))

(provide 'shellter-consult)

;;; shellter-consult.el ends here