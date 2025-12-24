;;; ai-common.el --- AI common conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(defun get-llm-api-host (env-var default-host)
  "Retrieve the LLM API host from an environment variable.
ENV-VAR is the name of the environment variable.
DEFAULT-HOST is the fallback value if ENV-VAR is not set or empty.
Returns the host string."
  (let ((host (getenv env-var)))
    (if (or (not host) (string-empty-p host)) default-host host)))

(defun get-llm-api-token (pass-var env-var)
  "Retrieve the LLM API token from a password manager or an environment variable.
PASS-VAR is the key for the token stored in GNU Pass.
ENV-VAR is the environment variable to use as a fallback if PASS-VAR is empty.
Returns the token string."
  (let ((token (string-trim (shell-command-to-string (format "pass %s" pass-var)))))
    (if (or (not token) (string-empty-p token))
        (getenv env-var) token)))

(defvar +llm-api-keys
  `((openai
     :token ,(lambda () (get-llm-api-token "openai/token" "OPENAI_API_KEY")))
    (chatanywhere
     :host ,(lambda () (get-llm-api-host "CHATANYWHERE_API_HOST" "api.chatanywhere.tech"))
     :token ,(lambda () (get-llm-api-token "chatanywhere/token" "CHATANYWHERE_API_KEY")))
    (gemini
     :token ,(lambda () (get-llm-api-token "gemini/token" "GEMINI_API_KEY")))
    (wq
     :host ,(lambda () (get-llm-api-host "WQ_API_HOST" "wanqing.streamlakeapi.com/api/gateway/v1/endpoints/ep-{{EP_ID}}/claude-code-proxy"))
     :ep ,(lambda () (get-llm-api-token "wq/ep" "WQ_EP_ID"))
     :token ,(lambda () (get-llm-api-token "wq/token" "WQ_API_KEY")))
    (minyuchat
     :host ,(lambda () (get-llm-api-host "MINYUCHAT_API_HOST" "chat.wminyu.top:433"))
     :ds-host ,(lambda () (get-llm-api-host "MINYUCHAT_DS_API_HOST" "chat.wminyu.top:433/v1"))
     :token ,(lambda () (get-llm-api-token "deepseek/web-token" "DEEPSEEK_WEB_API_KEY"))))
  "Alist of LLM API backend configurations.
Each entry is a list (BACKEND-NAME :KEY-TYPE (lambda () (GET-FUNCTION ...))).
Supported KEY-TYPEs are :host, :ds-host, and :token.")

(defvar +llm-api-keys-cache (make-hash-table :test 'equal)
  "Cache for storing already loaded llm api keys.")

(defun get-llm-api-key (backend key)
  "Retrieve property value for a LLM API, caching the result after first load.
BACKEND is the name of the LLM (e.g. gemini, chatanywhere, ...).
KEY is the property to retrieve (:host, :ds-host, or :token).
Returns the key value, or nil if not found.
If a token or host is not found, a warning message is displayed."
  (let ((cache-key (cons backend key)))
    (or (gethash cache-key +llm-api-keys-cache)
        (let* ((entry (alist-get backend +llm-api-keys))
               (val (plist-get entry key))
               (result (when val (funcall val))))
          (when result
            (puthash cache-key result +llm-api-keys-cache))
          result))))

;; key 不大可能变动，不常需要，就不设置快捷键了
(defun clear-llm-cache ()
  "Clear the LLM API keys cache. Useful when API keys are updated."
  (interactive)
  (clrhash +llm-api-keys-cache)
  (setq +gptel--backend-setup-done nil
        +aider--backend-setup-done nil)
  (message "LLM cache cleared, backends will be reinitialized"))

(provide 'ai-common)

;;; ai-common.el ends here.
