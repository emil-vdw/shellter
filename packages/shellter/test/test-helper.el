;;; test-helper.el --- Test helper for shellter -*- lexical-binding: t -*-

;;; Commentary:

;; Test helper for shellter package tests.

;;; Code:

(require 'ert)

;; Set up load path
(defvar shellter-test-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar shellter-root-path
  (expand-file-name ".." shellter-test-path))

(add-to-list 'load-path shellter-root-path)

;; Load shellter
(require 'shellter)

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