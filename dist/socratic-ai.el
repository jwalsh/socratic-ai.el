;;; socratic-ai.el --- Generate Socratic dialogues about buffer contents -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Jason Walsh
;; Author: Jason Walsh <j@wal.sh>
;; URL: https://github.com/jwalsh/socratic-ai.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, llm

;;; Commentary:

;; Read the current buffer and use Ollama to generate a Socratic
;; dialogue examining its contents through questioning.

;;; Code:

(require 'json)
(require 'url)

(defgroup socratic-ai nil
  "Generate Socratic dialogues about buffer contents."
  :group 'tools
  :prefix "socratic-ai-")

(defcustom socratic-ai-model "mistral"
  "Ollama model to use for generating dialogues."
  :type 'string
  :group 'socratic-ai)

(defcustom socratic-ai-endpoint "http://localhost:11434"
  "Ollama API endpoint."
  :type 'string
  :group 'socratic-ai)

(defcustom socratic-ai-system-prompt
  "You are Socrates. Examine the following text through the elenctic method. \
Ask probing questions that reveal assumptions, contradictions, or unstated premises. \
Do not summarize or explain. Only ask questions that would lead the author to \
examine their own thinking more deeply. Be concise."
  "System prompt defining the Socratic examination style."
  :type 'string
  :group 'socratic-ai)

(defcustom socratic-ai-buffer-name "*Socratic Dialogue*"
  "Name of the buffer for displaying dialogues."
  :type 'string
  :group 'socratic-ai)

(defun socratic-ai--ollama-available-p ()
  "Return non-nil if Ollama is reachable."
  (condition-case nil
      (let ((url-request-method "GET")
            (url-show-status nil))
        (with-current-buffer
            (url-retrieve-synchronously
             (concat socratic-ai-endpoint "/api/tags")
             t nil 5)
          (goto-char (point-min))
          (search-forward "200" nil t)))
    (error nil)))

(defun socratic-ai--call-ollama (text callback)
  "Send TEXT to Ollama and call CALLBACK with the response."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string
           (json-encode
            `((model . ,socratic-ai-model)
              (prompt . ,text)
              (system . ,socratic-ai-system-prompt)
              (stream . :json-false)))
           'utf-8))
         (endpoint (concat socratic-ai-endpoint "/api/generate")))
    (url-retrieve
     endpoint
     (lambda (status callback-fn)
       (if (plist-get status :error)
           (funcall callback-fn nil (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "^$" nil t)
         (let* ((json-object-type 'alist)
                (response (json-read)))
           (funcall callback-fn (alist-get 'response response) nil))))
     (list callback))))

(defun socratic-ai--display-response (response)
  "Display RESPONSE in a side window."
  (let ((buf (get-buffer-create socratic-ai-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert response)
        (goto-char (point-min)))
      (setq-local buffer-read-only t))
    (display-buffer-in-side-window buf '((side . right) (window-width . 0.4)))))

;;;###autoload
(defun socratic-ai-examine-buffer ()
  "Examine the current buffer through Socratic questioning."
  (interactive)
  (unless (socratic-ai--ollama-available-p)
    (warn "Ollama is not running at %s" socratic-ai-endpoint)
    (user-error "Ollama not available"))
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p (string-trim text))
      (user-error "Buffer is empty"))
    (message "Consulting Socrates...")
    (socratic-ai--call-ollama
     text
     (lambda (response error)
       (if error
           (warn "Ollama error: %s" error)
         (socratic-ai--display-response response)
         (message "Socratic examination complete."))))))

(provide 'socratic-ai)
;;; socratic-ai.el ends here
