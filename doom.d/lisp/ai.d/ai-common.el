;;; ai-common.el --- AI common conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(defun +llm--get-host (env-var default-host)
  "Retrieve the LLM API host from an environment variable.
ENV-VAR is the name of the environment variable.
DEFAULT-HOST is the fallback value if ENV-VAR is not set or empty.
Returns the host string."
  (let ((host (getenv env-var)))
    (if (or (not host) (string-empty-p host)) default-host host)))

(defun +llm--get-token (pass-var env-var)
  "Retrieve the LLM API token from a password manager or an environment variable.
PASS-VAR is the key for the token stored in GNU Pass.
ENV-VAR is the environment variable to use as a fallback if PASS-VAR is empty.
Returns the token string."
  (let ((token (string-trim (shell-command-to-string (format "pass %s" pass-var)))))
    (if (or (not token) (string-empty-p token))
        (getenv env-var) token)))

(defvar +llm-providers
  `((gemini
     :token ,(lambda () (+llm--get-token "gemini/token" "GEMINI_API_KEY")))
    (chatanywhere
     :host ,(lambda () (+llm--get-host "CHATANYWHERE_API_HOST" "api.chatanywhere.tech"))
     :token ,(lambda () (+llm--get-token "chatanywhere/token" "CHATANYWHERE_API_KEY")))
    (nvidia
     :host ,(lambda () (+llm--get-host "NVIDIA_NIM_API_HOST" "integrate.api.nvidia.com"))
     :token ,(lambda () (+llm--get-token "nvidia/token" "NVIDIA_NIM_API_KEY")))
    (zai
     :host ,(lambda () (+llm--get-host "ZAI_API_HOST" "open.bigmodel.cn/api/paas/v4"))
     :claude-host ,(lambda () (+llm--get-host "ZAI_CLAUDE_API_HOST" "open.bigmodel.cn/api/anthropic"))
     :token ,(lambda () (+llm--get-token "zai/token" "ZAI_API_KEY")))
    (ccr
     :host ,(lambda () (+llm--get-host "CCR_API_HOST" "127.0.0.1:3456"))
     :token ,(lambda () (+llm--get-token "ccr/token" "CCR_API_KEY"))
     :protocol "http")
    (minyuchat
     :host ,(lambda () (+llm--get-host "MINYUCHAT_API_HOST" "openai.wminyu.top:433"))
     :token ,(lambda () (+llm--get-token "deepseek/web-token" "DEEPSEEK_WEB_API_KEY"))))
  "Alist of LLM API backend configurations.
Each entry is a list (BACKEND-NAME :KEY-TYPE (lambda () (GET-FUNCTION ...))).
Supported KEY-TYPEs are :host, :claude-host, :token and :protocol.")

(defvar +llm-providers-cache (make-hash-table :test 'equal)
  "Cache for storing already loaded LLM API keys.")

(defun +llm-get-provider-conf (backend key)
  "Retrieve property value for a LLM provider, caching the result after first load.
BACKEND is the name of the LLM provider (e.g. gemini, chatanywhere, ...).
KEY is the property to retrieve (:host, :token...).
Returns the key value, or nil if not found."
  (let ((cache-key (cons backend key)))
    (or (gethash cache-key +llm-providers-cache)
        (let* ((entry (alist-get backend +llm-providers))
               (val (plist-get entry key))
               (result (cond ((functionp val) (funcall val))
                             (t val))))
          (when result
            (puthash cache-key result +llm-providers-cache))
          result))))

(defun +llm-get-provider-protocol (backend)
  "Get protocol for a LLM provider(default https).
BACKEND is the name of the LLM provider (e.g. gemini, chatanywhere, ...)."
  (or (+llm-get-provider-conf backend :protocol) "https"))

;; key 不大可能变动，不常需要，就不设置快捷键了
(defvar +llm-cache-clearers nil
  "Cache clearer function list of ai tool module.")

(defun +llm-clear-cache ()
  "Clear the LLM provider keys cache. Useful when API keys are updated."
  (interactive)
  (clrhash +llm-providers-cache)
  (dolist (clearer +llm-cache-clearers)
    (funcall clearer))
  (message "LLM cache cleared, backends will be reinitialized"))

(provide 'ai-common)

;;; ai-common.el ends here.
