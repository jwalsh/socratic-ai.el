;;; test-socratic-ai.el --- Tests for socratic-ai -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for socratic-ai.el

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'socratic-ai)

(ert-deftest socratic-ai-test-customization-defaults ()
  "Test that customization variables have sensible defaults."
  (should (stringp socratic-ai-model))
  (should (stringp socratic-ai-endpoint))
  (should (stringp socratic-ai-system-prompt))
  (should (string-match-p "localhost" socratic-ai-endpoint))
  (should (string-match-p "Socrates" socratic-ai-system-prompt)))

(ert-deftest socratic-ai-test-buffer-name ()
  "Test the dialogue buffer name."
  (should (string= socratic-ai-buffer-name "*Socratic Dialogue*")))

(ert-deftest socratic-ai-test-empty-buffer-error ()
  "Test that examining an empty buffer signals an error."
  (with-temp-buffer
    (should-error (socratic-ai-examine-buffer) :type 'user-error)))

(provide 'test-socratic-ai)
;;; test-socratic-ai.el ends here
