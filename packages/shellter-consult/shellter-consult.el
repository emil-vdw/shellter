;;; shellter-consult.el --- Consult integration for shellter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Emil van der Westhuizen

;; Author: Emil van der Westhuizen <emilvdw@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (shellter "0.1.0") (consult "0.35"))
;; Keywords: convenience, terminals, consult
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

;; Declare additional consult internals we need
(defvar consult--buffer-display)

(defun shellter-read-session-consult (sessions)
  "Read a session name from SESSIONS using `consult--read'.

This function provides enhanced features when consult is available:
- Live preview of session buffers
- Respects `shellter-switch-buffer-behaviour' during preview"
  (let* ((table
          (mapcar (lambda (session)
                    (cons (shellter-session-name session) session))
                  sessions))
         (prompt "Shellter session: ")
         ;; Determine display function based on configuration
         (display-fn (let ((is-shellter-buf (shellter-buffer-p)))
                       (pcase shellter-switch-buffer-behaviour
                         ('same-window #'switch-to-buffer)
                         ('other-window #'switch-to-buffer-other-window)
                         ('smart (if is-shellter-buf
                                     #'switch-to-buffer
                                   #'switch-to-buffer-other-window)))))
         ;; Create state function for preview
         (state
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
                     ;; Use consult's buffer display mechanism with our chosen function
                     (let ((consult--buffer-display display-fn))
                       (consult--buffer-action buffer))))))))))
    (consult--read table
                   :prompt prompt
                   :category 'shellter-session
                   :require-match t
                   :state state
                   :preview-key 'any
                   :sort nil)))

(provide 'shellter-consult)

;;; shellter-consult.el ends here
