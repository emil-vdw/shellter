;;; shellter-test.el --- Tests for shellter -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for shellter package.

;;; Code:

(require 'test-helper (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest shellter-test-context-provider ()
  "Test that the context provider can be set and retrieved."
  (let ((original-provider shellter-context-provider))
    (unwind-protect
        (progn
          ;; Test default provider
          (should (functionp shellter-context-provider))
          (should (shellter-context-p (funcall shellter-context-provider)))

          ;; Test setting a new provider
          (let ((custom-context (make-instance 'shellter-global-context)))
            (setq shellter-context-provider (lambda () custom-context))
            (should (eq (shellter-get-current-context) custom-context))))
      ;; Restore original provider
      (setq shellter-context-provider original-provider))))

(ert-deftest shellter-test-session-creation ()
  "Test creating and managing sessions."
  (let ((session (shellter-create-session "test-session")))
    (should (shellter-session-p session))
    (should (equal (shellter-session-name session) "test-session"))
    ;; Buffer should be created automatically now
    (should (bufferp (shellter-session-buffer session)))
    (should (buffer-live-p (shellter-session-buffer session)))
    ;; Clean up
    (kill-buffer (shellter-session-buffer session))))

(ert-deftest shellter-test-session-buffer-tracking ()
  "Test session buffer tracking."
  ;; Test with explicitly provided buffer
  (let* ((buffer (generate-new-buffer "*test-eshell*"))
         (session (shellter-create-session "test" buffer)))
    (unwind-protect
        (progn
          ;; Session with live buffer should be live
          (should (shellter-session-live-p session)))
      (kill-buffer buffer))

    ;; After killing buffer, session should not be live
    (should-not (shellter-session-live-p session)))

  ;; Test auto-created buffer cleanup
  (let ((session (shellter-create-session "test-auto")))
    (should (shellter-session-live-p session))
    (kill-buffer (shellter-session-buffer session))
    (should-not (shellter-session-live-p session))))

(ert-deftest shellter-test-global-context ()
  "Test the global context implementation."
  (let ((context (make-instance 'shellter-global-context)))
    ;; Should start empty
    (should (null (shellter-context-get-sessions context)))

    ;; Add a session
    (let ((session1 (shellter-create-session "session1")))
      (shellter-context-add-session context session1)
      (should (= 1 (length (shellter-context-get-sessions context))))
      (should (shellter-context-session-exists-p context "session1"))

      ;; Add another session
      (let ((session2 (shellter-create-session "session2")))
        (shellter-context-add-session context session2)
        (should (= 2 (length (shellter-context-get-sessions context))))

        ;; Remove a session
        (shellter-context-remove-session context session1)
        (should (= 1 (length (shellter-context-get-sessions context))))
        (should-not (shellter-context-session-exists-p context "session1"))
        (should (shellter-context-session-exists-p context "session2"))))))

(ert-deftest shellter-test-switch-single-session ()
  "Test switching when there's only one session."
  (shellter-test-with-context (make-instance 'shellter-global-context)
    (let* ((context (shellter-get-current-context))
           (session (shellter-create-session "only-session"))
           (switched-to nil))
      (unwind-protect
          (progn
            ;; Mock the switch function
            (cl-letf (((symbol-function 'shellter-switch-to-session)
                       (lambda (s) (setq switched-to s))))
              ;; Add the session
              (shellter-context-add-session context session)

              ;; Call shellter without prefix arg
              (shellter)

              ;; Should switch to the only session
              (should (eq switched-to session))))
        ;; Clean up
        (when (shellter-session-buffer session)
          (kill-buffer (shellter-session-buffer session)))))))

(ert-deftest shellter-test-create-with-prefix ()
  "Test creating new session with prefix argument."
  (shellter-test-with-context (make-instance 'shellter-global-context)
    (let* ((context (shellter-get-current-context))
           (existing-session (shellter-create-session "existing"))
           (created-session nil)
           (initial-count 0))
      (unwind-protect
          (progn
            ;; Mock functions
            (cl-letf (((symbol-function 'shellter-switch-to-session)
                       (lambda (s) (setq created-session s))))
              ;; Add existing session
              (shellter-context-add-session context existing-session)
              (setq initial-count (length (shellter-context-get-sessions context)))

              ;; Call with prefix arg
              (shellter '(4))

              ;; Should create new session
              (should created-session)
              (should-not (eq created-session existing-session))
              (should (= (length (shellter-context-get-sessions context)) 2))))
        ;; Clean up
        (when (shellter-session-buffer existing-session)
          (kill-buffer (shellter-session-buffer existing-session)))
        (when (and created-session (shellter-session-buffer created-session))
          (kill-buffer (shellter-session-buffer created-session)))))))

(ert-deftest shellter-test-completion-selection ()
  "Test session selection with completion."
  (shellter-test-with-context (make-instance 'shellter-global-context)
    (let* ((context (shellter-get-current-context))
           (session1 (shellter-create-session "proj1"))
           (session2 (shellter-create-session "proj2"))
           (selected nil))
      (unwind-protect
          (progn
            ;; Mock functions
            (cl-letf (((symbol-function 'shellter-switch-to-session)
                       (lambda (s) (setq selected s)))
                      ((symbol-function 'completing-read)
                       (lambda (prompt coll &rest args) "proj2")))
              ;; Add sessions
              (shellter-context-add-session context session1)
              (shellter-context-add-session context session2)

              ;; Call shellter
              (shellter)

              ;; Should find and switch to existing session2
              (should (equal (shellter-session-name selected) "proj2"))))
        ;; Clean up
        (when (shellter-session-buffer session1)
          (kill-buffer (shellter-session-buffer session1)))
        (when (shellter-session-buffer session2)
          (kill-buffer (shellter-session-buffer session2)))))))

(ert-deftest shellter-test-new-session-from-completion ()
  "Test creating new session from non-matching completion input."
  (shellter-test-with-context (make-instance 'shellter-global-context)
    (let* ((context (shellter-get-current-context))
           (existing1 (shellter-create-session "existing1"))
           (existing2 (shellter-create-session "existing2"))
           (switched-to nil))
      (unwind-protect
          (progn
            ;; Mock functions
            (cl-letf (((symbol-function 'shellter-switch-to-session)
                       (lambda (s) (setq switched-to s)))
                      ((symbol-function 'completing-read)
                       (lambda (prompt coll &rest args) "new-session")))
              ;; Add multiple existing sessions to trigger completion
              (shellter-context-add-session context existing1)
              (shellter-context-add-session context existing2)

              ;; Call shellter
              (shellter)

              ;; Should create and switch to new session
              (should switched-to)
              (should (equal (shellter-session-name switched-to) "new-session"))
              (should (= 3 (length (shellter-context-get-sessions context))))))
        ;; Clean up
        (when (shellter-session-buffer existing1)
          (kill-buffer (shellter-session-buffer existing1)))
        (when (shellter-session-buffer existing2)
          (kill-buffer (shellter-session-buffer existing2)))
        (when (and switched-to (shellter-session-buffer switched-to))
          (kill-buffer (shellter-session-buffer switched-to)))))))

(ert-deftest shellter-test-with-purpose ()
  "Test shellter with purpose argument."
  (shellter-test-with-context (make-instance 'shellter-global-context)
    (let* ((context (shellter-get-current-context))
           (created nil))
      (unwind-protect
          (progn
            ;; Mock functions
            (cl-letf (((symbol-function 'shellter-switch-to-session)
                       (lambda (s) (setq created s))))
              ;; Call with purpose
              (shellter nil "python-repl")

              ;; Should create session with purpose
              (should created)
              (should (equal (shellter-session-purpose created) "python-repl"))))
        ;; Clean up
        (when (and created (shellter-session-buffer created))
          (kill-buffer (shellter-session-buffer created)))))))

;; Clean up after all tests
(add-hook 'ert-runner-reporter-run-ended-functions
          #'shellter-test-cleanup)

(provide 'shellter-test)
;;; shellter-test.el ends here
