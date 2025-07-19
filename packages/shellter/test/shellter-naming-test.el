;;; shellter-naming-test.el --- Tests for shellter naming strategies -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for shellter naming strategies.

;;; Code:

(require 'test-helper (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))))
(require 'shellter-naming)
(require 'shellter-context)

;;; Command-based naming strategy tests

(ert-deftest shellter-command-naming-test-extract-command-name ()
  "Test extracting command names from various eshell command objects."
  ;; Test string input
  (should (equal (shellter-command-naming--extract-command-name "ls") "ls"))

  ;; Test function symbol
  (should (equal (shellter-command-naming--extract-command-name 'eshell/cd) "cd"))

  ;; Test printed representation
  (let ((printed-func "#<function eshell/ls>"))
    (should (equal (shellter-command-naming--extract-command-name printed-func) "ls")))

  ;; Test fallback
  (should (equal (shellter-command-naming--extract-command-name nil) "shell")))

(ert-deftest shellter-command-naming-test-relative-directory ()
  "Test getting relative directory path."
  (let ((home (expand-file-name "~")))
    ;; Test home directory
    (let ((default-directory home))
      (should (equal (shellter-command-naming--get-relative-directory) "~")))

    ;; Test subdirectory of home
    (let ((default-directory (expand-file-name "~/Development/project")))
      (should (equal (shellter-command-naming--get-relative-directory) "~/Development/project")))

    ;; Test directory outside home
    (let ((default-directory "/tmp"))
      (should (equal (shellter-command-naming--get-relative-directory) "/tmp")))))

(ert-deftest shellter-command-naming-test-format-name ()
  "Test name formatting."
  ;; Without purpose
  (should (equal (shellter-command-naming--format-name nil "ls" "~/projects")
                 "ls@~/projects"))

  ;; With purpose
  (should (equal (shellter-command-naming--format-name "python repl" "python" "~/dev")
                 "[python repl]: python@~/dev")))

(ert-deftest shellter-command-naming-test-generate-name ()
  "Test name generation with naming strategy."
  (let* ((strategy (make-instance 'shellter-command-naming-strategy))
         (context (shellter-create-naming-context
                   :directory "~/test"
                   :existing-names '("eshell@~/test"))))
    ;; First name should use fallback command
    (should (equal (shellter-generate-name strategy context) "eshell@~/test<2>"))

    ;; Test with purpose
    (let ((context-with-purpose (shellter-create-naming-context
                                 :directory "~/test"
                                 :purpose "build"
                                 :existing-names '())))
      (should (equal (shellter-generate-name strategy context-with-purpose)
                     "[build]: eshell@~/test")))))

(ert-deftest shellter-command-naming-test-update-name ()
  "Test dynamic name updates."
  (let* ((buffer (generate-new-buffer "*test-eshell*"))
         (session (shellter-create-session "test" buffer))
         (strategy (make-instance 'shellter-command-naming-strategy)))
    (unwind-protect
        (with-current-buffer buffer
          ;; Set up eshell mode
          (eshell-mode)

          ;; Initially no update without command
          (let ((context (shellter-create-naming-context :existing-names '())))
            (should-not (shellter-update-name strategy session context)))

          ;; Set last command and test update
          (setq eshell-last-command-name 'eshell/ls)
          (let ((context (shellter-create-naming-context :existing-names '())))
            (let ((new-name (shellter-update-name strategy session context)))
              (should (stringp new-name))
              (should (string-match "^ls@" new-name))))

          ;; Test with purpose
          (setf (shellter-session-purpose session) "test run")
          (let ((context (shellter-create-naming-context :existing-names '())))
            (let ((new-name (shellter-update-name strategy session context)))
              (should (stringp new-name))
              (should (string-match "^\\[test run\\]: ls@" new-name)))))
      (kill-buffer buffer))))

(ert-deftest shellter-command-naming-test-provider-setup ()
  "Test setting up command naming strategy via provider."
  (let ((original-provider shellter-naming-strategy-provider))
    (unwind-protect
        (progn
          ;; Set command naming strategy
          (setq shellter-naming-strategy-provider #'shellter-command-naming-provider)
          (should (cl-typep (shellter-get-naming-strategy) 'shellter-command-naming-strategy))

          ;; Users need to manually add the hook if they want dynamic updates
          (add-hook 'eshell-post-command-hook #'shellter-command-naming--update-session-name)
          (should (member #'shellter-command-naming--update-session-name eshell-post-command-hook))

          ;; Switch back to simple strategy
          (setq shellter-naming-strategy-provider #'shellter-simple-naming-provider)
          (should-not (cl-typep (shellter-get-naming-strategy) 'shellter-command-naming-strategy)))
      ;; Restore original state
      (setq shellter-naming-strategy-provider original-provider)
      (remove-hook 'eshell-post-command-hook #'shellter-command-naming--update-session-name))))

;;; Simple naming strategy tests

(ert-deftest shellter-naming-test-singleton-providers ()
  "Test that providers return singleton instances."
  ;; Test simple naming provider
  (let ((instance1 (shellter-simple-naming-provider))
        (instance2 (shellter-simple-naming-provider)))
    (should (eq instance1 instance2)))

  ;; Test directory naming provider
  (let ((instance1 (shellter-directory-naming-provider))
        (instance2 (shellter-directory-naming-provider)))
    (should (eq instance1 instance2)))

  ;; Test command naming provider
  (let ((instance1 (shellter-command-naming-provider))
        (instance2 (shellter-command-naming-provider)))
    (should (eq instance1 instance2))))

(ert-deftest shellter-simple-naming-test-generate ()
  "Test simple incremental name generation."
  (let* ((strategy (make-instance 'shellter-simple-naming-strategy))
         (context (shellter-create-naming-context
                   :existing-names '("eshell" "eshell<2>"))))
    (should (equal (shellter-generate-name strategy context) "eshell<3>")))

  ;; Test with base name
  (let* ((strategy (make-instance 'shellter-simple-naming-strategy))
         (context (shellter-create-naming-context
                   :base-name "term"
                   :existing-names '("term"))))
    (should (equal (shellter-generate-name strategy context) "term<2>"))))

;;; Directory naming strategy tests

(ert-deftest shellter-directory-naming-test-generate ()
  "Test directory-based name generation."
  (let* ((strategy (make-instance 'shellter-directory-naming-strategy))
         (context (shellter-create-naming-context
                   :directory "/home/user/projects/myapp"
                   :existing-names '())))
    (should (equal (shellter-generate-name strategy context) "eshell@myapp")))

  ;; Test with parent directory
  (let* ((strategy (make-instance 'shellter-directory-naming-strategy
                                   :include-parent t))
         (context (shellter-create-naming-context
                   :directory "/home/user/projects/myapp"
                   :existing-names '())))
    (should (equal (shellter-generate-name strategy context) "eshell@projects/myapp"))))

(ert-deftest shellter-directory-naming-test-update ()
  "Test directory name updates."
  (let* ((buffer (generate-new-buffer "*test-eshell*"))
         (session (shellter-create-session "eshell@old" buffer))  ; Use name that matches directory
         (strategy (make-instance 'shellter-directory-naming-strategy)))
    (unwind-protect
        (progn
          ;; Test that name changes when context directory changes
          (let ((old-context (shellter-create-naming-context
                             :directory "/tmp/old"
                             :existing-names '()))
                (new-context (shellter-create-naming-context
                             :directory "/tmp/new"
                             :existing-names '())))
            ;; Same directory, no update needed since name already matches
            (should-not (shellter-update-name strategy session old-context))
            ;; Different directory, should update
            (should (equal (shellter-update-name strategy session new-context) "eshell@new"))))
      (kill-buffer buffer))))

;;; Utility function tests

(ert-deftest shellter-naming-test-make-unique ()
  "Test name uniqueness helper."
  (should (equal (shellter--make-unique-name "test" '()) "test"))
  (should (equal (shellter--make-unique-name "test" '("test")) "test<2>"))
  (should (equal (shellter--make-unique-name "test" '("test" "test<2>")) "test<3>")))

(ert-deftest shellter-naming-test-context-creation ()
  "Test naming context creation."
  (let ((context (shellter-create-naming-context
                  :base-name "base"
                  :directory "/tmp"
                  :project-root "/home/project"
                  :existing-names '("a" "b")
                  :purpose "testing"
                  :current-name "old-name"
                  :metadata '((key . value)))))
    (should (equal (shellter-naming-context-base-name context) "base"))
    (should (equal (shellter-naming-context-directory context) "/tmp"))
    (should (equal (shellter-naming-context-project-root context) "/home/project"))
    (should (equal (shellter-naming-context-existing-names context) '("a" "b")))
    (should (equal (shellter-naming-context-purpose context) "testing"))
    (should (equal (shellter-naming-context-current-name context) "old-name"))
    (should (equal (shellter-naming-context-metadata context) '((key . value))))))

(ert-deftest shellter-naming-test-deduplication-excludes-self ()
  "Test that session name updates don't deduplicate against themselves."
  (let* ((buffer (generate-new-buffer "*test-eshell*"))
         (session (shellter-create-session "ls@~/test" buffer))
         (strategy (make-instance 'shellter-command-naming-strategy))
         ;; Add the session to the global context
         (context (shellter-get-current-context)))
    (unwind-protect
        (progn
          (shellter-context-add-session context session)
          (with-current-buffer buffer
            ;; Set up eshell mode
            (eshell-mode)
            ;; Set the same command that would result in the same name
            (setq eshell-last-command-name 'eshell/ls)
            (let ((default-directory "~/test"))
              ;; Get naming context with current session name
              (let ((naming-context (shellter-get-current-naming-context nil nil "ls@~/test")))
                ;; The name should remain unchanged, not become "ls@~/test<2>"
                (should-not (shellter-update-name strategy session naming-context))))))
      (shellter-context-remove-session context session)
      (kill-buffer buffer))))

(ert-deftest shellter-naming-test-make-unique-with-current ()
  "Test that shellter--make-unique-name handles current-name correctly."
  ;; When base-name equals current-name, return as-is
  (should (equal (shellter--make-unique-name "test" '("test" "other") "test")
                 "test"))

  ;; When base-name differs from current-name, deduplicate normally
  (should (equal (shellter--make-unique-name "test" '("test" "other") "different")
                 "test<2>"))

  ;; When current-name is nil, deduplicate normally
  (should (equal (shellter--make-unique-name "test" '("test" "other") nil)
                 "test<2>")))

(provide 'shellter-naming-test)
;;; shellter-naming-test.el ends here
