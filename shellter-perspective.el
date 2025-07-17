;;; shellter-perspective.el --- Perspective.el integration for shellter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Emil van der Westhuizen

;; Author: Emil van der Westhuizen <emilvdw@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (shellter "0.1.0") (perspective "2.8"))
;; Keywords: convenience, terminals, perspective
;; URL: https://github.com/emil-vdw/shellter

;;; Commentary:

;; This package provides perspective.el integration for shellter, allowing
;; eshell sessions to be managed per-perspective rather than globally.
;;
;; When using this integration:
;; - Each perspective maintains its own list of eshell sessions
;; - Sessions are tracked by their buffers in perspectives
;; - Switching perspectives shows only relevant sessions
;;
;; To use this integration, add the following to your Emacs configuration:
;;
;;   (require 'shellter-perspective)
;;   (setq shellter-context-provider #'shellter-perspective-context-provider)

;;; Code:

(require 'shellter)
(require 'cl-lib)

;; Require perspective - this file won't work without it
(unless (require 'perspective nil t)
  (error "Package 'perspective' is required for shellter-perspective"))

;;; Custom Variables

(defgroup shellter-perspective nil
  "Perspective integration for shellter."
  :group 'shellter
  :prefix "shellter-perspective-")

(defcustom shellter-perspective-inherit-global-sessions nil
  "If non-nil, new perspectives inherit sessions from the global context."
  :type 'boolean
  :group 'shellter-perspective)

;;; Internal Data Structures

(defvar shellter-perspective--all-sessions nil
  "List of all shellter sessions across all perspectives.")

;;; Perspective Context Implementation

(defclass shellter-perspective-context (shellter-context)
  ()
  "A perspective-aware context that manages sessions per perspective.
This context implementation integrates with perspective.el to provide
workspace-specific eshell session management.")

(cl-defmethod shellter-context-get-sessions ((context shellter-perspective-context))
  "Return all sessions associated with the current perspective."
  ;; Get buffers in current perspective
  (let ((persp-buffers (persp-buffers (persp-curr))))
    ;; Filter sessions whose buffers are in the current perspective
    (cl-remove-if-not
     (lambda (session)
       (member (shellter-session-buffer session) persp-buffers))
     shellter-perspective--all-sessions)))

(cl-defmethod shellter-context-add-session ((context shellter-perspective-context) session)
  "Add SESSION to the current perspective's context."
  (let ((buffer (shellter-session-buffer session)))
    ;; Add session to global list if not already there
    (unless (cl-find (shellter-session-name session)
                     shellter-perspective--all-sessions
                     :key #'shellter-session-name
                     :test #'string=)
      (push session shellter-perspective--all-sessions))

    ;; Add the buffer to the current perspective
    (when (and buffer (buffer-live-p buffer))
      (persp-add-buffer buffer))))

(cl-defmethod shellter-context-remove-session ((context shellter-perspective-context) session)
  "Remove SESSION from all perspectives."
  ;; Remove from global list
  (setq shellter-perspective--all-sessions
        (cl-remove session shellter-perspective--all-sessions
                   :test #'equal))

  ;; The buffer will be automatically removed from perspectives
  ;; when it's killed, so we don't need to do anything else here
  )

;;; Perspective Hooks

(defun shellter-perspective--cleanup-on-persp-kill ()
  "Clean up shellter sessions when a perspective is killed.
This function is run by `persp-killed-hook' with the perspective
to be killed as the current perspective."
  (let ((sessions (shellter-context-get-sessions (shellter-perspective-context-provider)))
        (current-persp-name (persp-current-name)))
    ;; Kill shellter buffers that exist only in this perspective
    (dolist (session sessions)
      (when-let ((buffer (shellter-session-buffer session)))
        (when (buffer-live-p buffer)
          ;; Check if buffer exists in any other perspective
          (let ((buffer-in-other-persp nil))
            (dolist (persp-name (persp-names))
              (unless (string= persp-name current-persp-name)
                (when (member buffer (persp-buffers (gethash persp-name (perspectives-hash))))
                  (setq buffer-in-other-persp t))))
            ;; Only kill if not in any other perspective
            (unless buffer-in-other-persp
              (kill-buffer buffer))))))
    ;; Clean up any dead sessions from the global list
    (setq shellter-perspective--all-sessions
          (cl-remove-if-not
           (lambda (session)
             (and (shellter-session-buffer session)
                  (buffer-live-p (shellter-session-buffer session))))
           shellter-perspective--all-sessions))))

(defun shellter-perspective--setup-hooks ()
  "Set up hooks for perspective integration."
  (add-hook 'persp-killed-hook #'shellter-perspective--cleanup-on-persp-kill))

(defun shellter-perspective--teardown-hooks ()
  "Remove hooks for perspective integration."
  (remove-hook 'persp-killed-hook #'shellter-perspective--cleanup-on-persp-kill))

;;; Variable Watcher

(defun shellter-perspective--context-provider-watcher (_symbol newval _op _where)
  "Watch for changes to `shellter-context-provider'.
When the provider is set to perspective context, set up hooks.
When switching away, tear down hooks."
  (cond
   ;; Setting to perspective provider
   ((eq newval #'shellter-perspective-context-provider)
    (shellter-perspective--setup-hooks))
   ;; Switching away from perspective provider - check if we were using it
   ((and (boundp 'shellter-context-provider)
         (eq shellter-context-provider #'shellter-perspective-context-provider))
    (shellter-perspective--teardown-hooks))))

;; Add the watcher when this file is loaded
(add-variable-watcher 'shellter-context-provider
                      #'shellter-perspective--context-provider-watcher)

;;; Provider Function

(defvar shellter-perspective--context-instance nil
  "The singleton instance of the perspective context.")

;;;###autoload
(defun shellter-perspective-context-provider ()
  "Return the perspective context instance for shellter.
This function is meant to be used as the value of `shellter-context-provider'."
  (unless shellter-perspective--context-instance
    (setq shellter-perspective--context-instance
          (make-instance 'shellter-perspective-context)))
  shellter-perspective--context-instance)

(provide 'shellter-perspective)

;; Local Variables:
;; package-lint-main-file: "shellter-perspective.el"
;; End:

;;; shellter-perspective.el ends here
