;;; test-helper.el --- Test helper for shellter-consult -*- lexical-binding: t -*-

;;; Commentary:

;; Test helper for shellter-consult package tests.

;;; Code:

(require 'ert)

;; Set up load path
(defvar shellter-consult-test-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar shellter-consult-root-path
  (expand-file-name ".." shellter-consult-test-path))

(defvar shellter-packages-path
  (expand-file-name "../.." shellter-consult-root-path))

;; Add shellter-consult to load path
(add-to-list 'load-path shellter-consult-root-path)

;; Add shellter (core) to load path
(add-to-list 'load-path (expand-file-name "shellter" shellter-packages-path))

;; Load dependencies
(require 'shellter)
(require 'shellter-consult)

;; Test utilities
(defun shellter-test-cleanup ()
  "Clean up after tests."
  ;; Kill any test eshell buffers
  (dolist (buffer (buffer-list))
    (when (and (buffer-name buffer)
               (string-match-p "\\*eshell:" (buffer-name buffer)))
      (ignore-errors (kill-buffer buffer)))))

(provide 'test-helper)
;;; test-helper.el ends here