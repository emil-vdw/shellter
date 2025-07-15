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
