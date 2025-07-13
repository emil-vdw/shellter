;;; shellter-perspective-test.el --- Tests for shellter-perspective -*- lexical-binding: t -*-

;;; Commentary:

;; Test suite for shellter-perspective integration.

;;; Code:

(require 'shellter-perspective)

(require 'test-helper (expand-file-name "test-helper" (file-name-directory (or load-file-name buffer-file-name))))

(defmacro shellter-perspective-test-with-perspectives (&rest body)
  "Execute BODY with a clean perspective setup."
  `(let ((persp-mode-enabled persp-mode)
         (old-provider shellter-context-provider)
         ;; Save original data
         (old-all-sessions shellter-perspective--all-sessions))
     (unwind-protect
         (progn
           ;; Enable perspective mode if not already
           (unless persp-mode (persp-mode 1))
           ;; Clear data structures
           (setq shellter-perspective--all-sessions nil)
           ;; Enable shellter-perspective
           (shellter-perspective-enable)
           ,@body)
       ;; Restore state
       (setq shellter-context-provider old-provider)
       (setq shellter-perspective--all-sessions old-all-sessions)
       (unless persp-mode-enabled (persp-mode -1)))))

(ert-deftest shellter-perspective-test-context-provider ()
  "Test that the perspective context provider works correctly."
  (shellter-perspective-test-with-perspectives
   (should (eq shellter-context-provider #'shellter-perspective-context-provider))
   (let ((context (shellter-get-current-context)))
     (should (shellter-context-p context))
     (should (cl-typep context 'shellter-perspective-context)))))

(ert-deftest shellter-perspective-test-session-isolation ()
  "Test that sessions are isolated between perspectives."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context))
         (main-persp (persp-current-name)))

     ;; Add a session to main perspective
     (let ((session1 (shellter-create-session "main-session")))
       (shellter-context-add-session context session1)
       (should (= 1 (length (shellter-context-get-sessions context))))

       ;; Create and switch to a new perspective
       (persp-switch "test-persp")

       ;; Should have no sessions in new perspective
       (should (= 0 (length (shellter-context-get-sessions context))))

       ;; Add a different session
       (let ((session2 (shellter-create-session "test-session")))
         (shellter-context-add-session context session2)
         (should (= 1 (length (shellter-context-get-sessions context))))

         ;; Switch back to main - should see only main session
         (persp-switch main-persp)
         (should (= 1 (length (shellter-context-get-sessions context))))
         (should (string= "main-session"
                          (shellter-session-name
                           (car (shellter-context-get-sessions context))))))))))

(ert-deftest shellter-perspective-test-buffer-sharing ()
  "Test that buffers can be shared across perspectives."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context))
         (main-persp (persp-current-name)))

     ;; Create session in main perspective
     (let ((session (shellter-create-session "shared-session")))
       (shellter-context-add-session context session)

       ;; Switch to new perspective
       (persp-switch "test-persp")

       ;; Add the buffer to this perspective too
       (persp-add-buffer (shellter-session-buffer session))

       ;; The session should now appear in this perspective's context
       (should (= 1 (length (shellter-context-get-sessions context))))
       (should (string= "shared-session"
                        (shellter-session-name
                         (car (shellter-context-get-sessions context)))))))))

(ert-deftest shellter-perspective-test-perspective-deletion ()
  "Test that sessions are cleaned up when perspectives are deleted."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context)))

     ;; Create a new perspective with a session
     (persp-switch "temp-persp")
     (let ((session (shellter-create-session "temp-session")))
       (shellter-context-add-session context session)
       
       ;; Verify session exists in global list
       (should (= 1 (length shellter-perspective--all-sessions)))

       ;; Kill the perspective
       (persp-kill "temp-persp")

       ;; Session should still exist in global list (not removed when perspective is killed)
       ;; but won't be visible in any perspective unless its buffer is added
       (should (= 1 (length shellter-perspective--all-sessions)))))))

(ert-deftest shellter-perspective-test-perspective-rename ()
  "Test that sessions are preserved when perspectives are renamed."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context)))

     ;; Create perspective with session
     (persp-switch "old-name")
     (let ((session (shellter-create-session "rename-test")))
       (shellter-context-add-session context session)

       ;; Rename the perspective
       (persp-rename "new-name")

       ;; Session should still be accessible
       (should (= 1 (length (shellter-context-get-sessions context))))
       (should (string= "rename-test"
                        (shellter-session-name
                         (car (shellter-context-get-sessions context)))))))))

(ert-deftest shellter-perspective-test-enable-disable ()
  "Test enabling and disabling perspective integration."
  (let ((original-provider shellter-context-provider))
    (unwind-protect
        (progn
          ;; Enable perspective integration
          (shellter-perspective-enable)
          (should (eq shellter-context-provider #'shellter-perspective-context-provider))

          ;; Disable perspective integration
          (shellter-perspective-disable)
          (should (eq shellter-context-provider #'shellter-global-context-provider)))
      ;; Restore original
      (setq shellter-context-provider original-provider))))



(provide 'shellter-perspective-test)
;;; shellter-perspective-test.el ends here