;;; test-socratic-ai.el --- Tests for socratic-ai -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for socratic-ai.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../src" (file-name-directory load-file-name)))
(require 'socratic-ai)

;;; Customization tests

(ert-deftest socratic-ai-test-customization-defaults ()
  "Test that customization variables have sensible defaults."
  (should (stringp socratic-ai-model))
  (should (stringp socratic-ai-endpoint))
  (should (stringp socratic-ai-system-prompt))
  (should (string-match-p "localhost" socratic-ai-endpoint))
  (should (string-match-p "11434" socratic-ai-endpoint)))

(ert-deftest socratic-ai-test-system-prompt-contains-socrates ()
  "Test that system prompt references Socratic method."
  (should (string-match-p "Socrates" socratic-ai-system-prompt)))

(ert-deftest socratic-ai-test-system-prompt-emphasizes-questions ()
  "Test that system prompt asks for questions, not summaries."
  (should (string-match-p "question" socratic-ai-system-prompt))
  (should (string-match-p "not summarize" socratic-ai-system-prompt)))

(ert-deftest socratic-ai-test-buffer-name ()
  "Test the dialogue buffer name."
  (should (string= socratic-ai-buffer-name "*Socratic Dialogue*")))

(ert-deftest socratic-ai-test-model-default ()
  "Test default model is reasonable."
  (should (member socratic-ai-model '("mistral" "llama2" "llama3" "llama3.2"))))

;;; Input validation tests

(ert-deftest socratic-ai-test-empty-buffer-error ()
  "Test that examining an empty buffer signals an error."
  (with-temp-buffer
    (should-error (socratic-ai-examine-buffer) :type 'user-error)))

(ert-deftest socratic-ai-test-whitespace-only-buffer-error ()
  "Test that a buffer with only whitespace signals an error."
  (with-temp-buffer
    (insert "   \n\t\n   ")
    (should-error (socratic-ai-examine-buffer) :type 'user-error)))

;;; Display tests

(ert-deftest socratic-ai-test-display-creates-buffer ()
  "Test that display function creates the output buffer."
  (let ((test-response "What assumptions underlie this claim?"))
    (socratic-ai--display-response test-response)
    (should (get-buffer socratic-ai-buffer-name))
    (with-current-buffer socratic-ai-buffer-name
      (should (string= (buffer-string) test-response))
      (should buffer-read-only))
    (kill-buffer socratic-ai-buffer-name)))

(ert-deftest socratic-ai-test-display-replaces-content ()
  "Test that display function replaces previous content."
  (let ((buf (get-buffer-create socratic-ai-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert "old content")))
    (socratic-ai--display-response "new content")
    (with-current-buffer socratic-ai-buffer-name
      (should (string= (buffer-string) "new content")))
    (kill-buffer socratic-ai-buffer-name)))

;;; Availability check tests

(ert-deftest socratic-ai-test-unavailable-ollama ()
  "Test that unavailable Ollama is detected."
  (let ((socratic-ai-endpoint "http://localhost:99999"))
    (should-not (socratic-ai--ollama-available-p))))

;;; JSON payload tests

(ert-deftest socratic-ai-test-json-encoding ()
  "Test that JSON payload structure is correct."
  (let* ((socratic-ai-model "test-model")
         (socratic-ai-system-prompt "test prompt")
         (payload (json-encode
                   `((model . ,socratic-ai-model)
                     (prompt . "test text")
                     (system . ,socratic-ai-system-prompt)
                     (stream . :json-false)))))
    (should (string-match-p "\"model\":\"test-model\"" payload))
    (should (string-match-p "\"system\":\"test prompt\"" payload))
    (should (string-match-p "\"stream\":false" payload))))

(provide 'test-socratic-ai)
;;; test-socratic-ai.el ends here
