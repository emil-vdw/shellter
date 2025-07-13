;;; shellter-test.el --- Tests for shellter -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for shellter package.

;;; Code:

(require 'test-helper)

(ert-deftest shellter-test-mode-activation ()
  "Test that shellter-mode can be activated and deactivated."
  (shellter-mode 1)
  (should shellter-mode)
  (shellter-mode -1)
  (should-not shellter-mode))

(ert-deftest shellter-test-perspective-name ()
  "Test perspective name detection."
  ;; Without perspective-mode
  (let ((persp-mode nil))
    (should (equal (shellter--perspective-name) "default")))
  
  ;; With perspective-mode
  (shellter-test-with-perspective
   (let ((persp-curr (make-hash-table :test 'equal)))
     (puthash "name" "test-perspective" persp-curr)
     (flet ((persp-current-name () "test-perspective"))
       (should (equal (shellter--perspective-name) "test-perspective"))))))

(ert-deftest shellter-test-buffer-name-generation ()
  "Test buffer name generation."
  (let ((shellter-session-name-format "*eshell: %s [%s]*"))
    (flet ((shellter--perspective-name () "test-persp"))
      (should (equal (shellter--make-buffer-name "main")
                     "*eshell: test-persp [main]*"))
      (should (equal (shellter--make-buffer-name "build")
                     "*eshell: test-persp [build]*")))))

(ert-deftest shellter-test-customization ()
  "Test customization variables."
  (should (equal shellter-default-session-name "main"))
  (should (equal shellter-session-name-format "*eshell: %s [%s]*"))
  
  ;; Test customization changes
  (let ((shellter-default-session-name "custom"))
    (should (equal shellter-default-session-name "custom"))))

;; Clean up after all tests
(add-hook 'ert-runner-reporter-run-ended-functions
          #'shellter-test-cleanup)

(provide 'shellter-test)
;;; shellter-test.el ends here