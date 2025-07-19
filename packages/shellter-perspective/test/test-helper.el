;;; test-helper.el --- Test helper for shellter-perspective -*- lexical-binding: t -*-

;;; Commentary:

;; Test helper for shellter-perspective package tests.

;;; Code:

(require 'ert)

;; Set up load path
(defvar shellter-perspective-test-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar shellter-perspective-root-path
  (expand-file-name ".." shellter-perspective-test-path))

(defvar shellter-packages-path
  (expand-file-name "../.." shellter-perspective-root-path))

;; Add shellter-perspective to load path
(add-to-list 'load-path shellter-perspective-root-path)

;; Add shellter (core) to load path
(add-to-list 'load-path (expand-file-name "shellter" shellter-packages-path))

;; Load dependencies
(require 'shellter)
(require 'shellter-perspective)

;; Test utilities
(defmacro shellter-test-with-context (context-expr &rest body)
  "Execute BODY with a specific CONTEXT as the current context."
  (let ((context-var (make-symbol "context")))
    `(let* ((,context-var ,context-expr)
            (shellter-context-provider (lambda () ,context-var)))
       ,@body)))

(defun shellter-test-cleanup ()
  "Clean up after tests."
  ;; Kill any shellter session buffers
  (dolist (buffer (buffer-list))
    (when (shellter-buffer-p buffer)
      (ignore-errors (kill-buffer buffer)))))

(provide 'test-helper)
;;; test-helper.el ends here