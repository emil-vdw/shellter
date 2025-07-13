;;; test-helper.el --- Test helper for shellter -*- lexical-binding: t -*-

;;; Commentary:

;; Test helper for shellter package tests.

;;; Code:

(require 'ert)
(require 'f)

;; Set up load path
(defvar shellter-test-path
  (f-dirname (f-this-file)))

(defvar shellter-root-path
  (f-parent shellter-test-path))

(add-to-list 'load-path shellter-root-path)

;; Load shellter
(require 'shellter)

;; Test utilities
(defmacro shellter-test-with-perspective (&rest body)
  "Execute BODY with a temporary perspective setup."
  `(let ((persp-mode t)
         (perspectives-hash (make-hash-table :test 'equal :size 10)))
     ,@body))

(defun shellter-test-cleanup ()
  "Clean up after tests."
  ;; Kill any test eshell buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\*eshell:" (buffer-name buffer))
      (kill-buffer buffer))))

(provide 'test-helper)
;;; test-helper.el ends here