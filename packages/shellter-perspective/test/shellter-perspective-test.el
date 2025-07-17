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
           (setq shellter-context-provider #'shellter-perspective-context-provider)
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
     (let ((session (shellter-create-session "shared-session"))
           (buffer nil))
       (shellter-context-add-session context session)
       (setq buffer (shellter-session-buffer session))

       ;; Switch to new perspective
       (persp-switch "test-persp")

       ;; Add the buffer to this perspective too
       (persp-add-buffer buffer)

       ;; The session should now appear in this perspective's context
       (should (= 1 (length (shellter-context-get-sessions context))))
       (should (string= "shared-session"
                        (shellter-session-name
                         (car (shellter-context-get-sessions context)))))

       ;; Kill the test perspective
       (persp-kill "test-persp")

       ;; Buffer should still be alive since it's also in main perspective
       (should (buffer-live-p buffer))

       ;; Switch back to main and verify session is still there
       (persp-switch main-persp)
       (should (= 1 (length (shellter-context-get-sessions context))))
       (should (buffer-live-p buffer))
       (should (string= "shared-session"
                        (shellter-session-name
                         (car (shellter-context-get-sessions context)))))))))

(ert-deftest shellter-perspective-test-perspective-deletion ()
  "Test that sessions are cleaned up when perspectives are deleted."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context))
         (main-persp (persp-current-name)))

     ;; Create a new perspective with a session
     (persp-switch "temp-persp")
     (let ((session (shellter-create-session "temp-session"))
           (buffer nil))
       (shellter-context-add-session context session)
       (setq buffer (shellter-session-buffer session))

       ;; Verify session exists in global list
       (should (= 1 (length shellter-perspective--all-sessions)))
       (should (buffer-live-p buffer))

       ;; Kill the perspective
       (persp-kill "temp-persp")

       ;; Session should be removed from global list and buffer should be killed
       (should (= 0 (length shellter-perspective--all-sessions)))
       (should-not (buffer-live-p buffer))))))

(ert-deftest shellter-perspective-test-selective-cleanup ()
  "Test that only sessions in the deleted perspective are cleaned up."
  (shellter-perspective-test-with-perspectives
   (let ((context (shellter-get-current-context))
         (main-persp (persp-current-name)))

     ;; Create session in main perspective
     (let ((main-session (shellter-create-session "main-session"))
           (main-buffer nil))
       (shellter-context-add-session context main-session)
       (setq main-buffer (shellter-session-buffer main-session))

       ;; Create a new perspective with its own session
       (persp-switch "temp-persp")
       (let ((temp-session (shellter-create-session "temp-session"))
             (temp-buffer nil))
         (shellter-context-add-session context temp-session)
         (setq temp-buffer (shellter-session-buffer temp-session))

         ;; Verify both sessions exist
         (should (= 2 (length shellter-perspective--all-sessions)))
         (should (buffer-live-p main-buffer))
         (should (buffer-live-p temp-buffer))

         ;; Kill the temp perspective
         (persp-kill "temp-persp")

         ;; Only temp session should be removed
         (should (= 1 (length shellter-perspective--all-sessions)))
         (should (buffer-live-p main-buffer))
         (should-not (buffer-live-p temp-buffer))

         ;; Switch back to main and verify session is still there
         (persp-switch main-persp)
         (should (= 1 (length (shellter-context-get-sessions context))))
         (should (string= "main-session"
                          (shellter-session-name
                           (car (shellter-context-get-sessions context))))))))))

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

(ert-deftest shellter-perspective-test-context-switching ()
  "Test switching between perspective and global context providers."
  (let ((original-provider shellter-context-provider))
    (unwind-protect
        (progn
          ;; Set perspective integration
          (setq shellter-context-provider #'shellter-perspective-context-provider)
          (should (eq shellter-context-provider #'shellter-perspective-context-provider))

          ;; Switch to global context provider
          (setq shellter-context-provider #'shellter-global-context-provider)
          (should (eq shellter-context-provider #'shellter-global-context-provider)))
      ;; Restore original
      (setq shellter-context-provider original-provider))))

(ert-deftest shellter-perspective-test-hook-management ()
  "Test that hooks are properly managed when switching context providers."
  (let ((original-provider shellter-context-provider)
        (original-hooks persp-killed-hook))
    (unwind-protect
        (progn
          ;; Start with no hooks
          (setq persp-killed-hook nil)

          ;; Set perspective provider - should add hook
          (setq shellter-context-provider #'shellter-perspective-context-provider)
          (should (memq #'shellter-perspective--cleanup-on-persp-kill persp-killed-hook))

          ;; Switch to global provider - should remove hook
          (setq shellter-context-provider #'shellter-global-context-provider)
          (should-not (memq #'shellter-perspective--cleanup-on-persp-kill persp-killed-hook))

          ;; Switch back to perspective - should add hook again
          (setq shellter-context-provider #'shellter-perspective-context-provider)
          (should (memq #'shellter-perspective--cleanup-on-persp-kill persp-killed-hook)))
      ;; Restore original state
      (setq shellter-context-provider original-provider)
      (setq persp-killed-hook original-hooks))))



(provide 'shellter-perspective-test)
;;; shellter-perspective-test.el ends here
