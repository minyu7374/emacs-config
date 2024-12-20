;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(defun get-api-token (pass-key env-key)
  "Retrieve API token from password manager or environment variable.
PASS-KEY is the command to retrieve the token from the password manager.
ENV-KEY is the environment variable to fallback if PASS-KEY fails."
  (let ((token (string-trim (shell-command-to-string (format "pass %s" pass-key)))))
    (if (or (not token) (string-empty-p token))
        (getenv env-key) token)))

(use-package! gptel
  :config
  (setq gptel-api-key (get-api-token "openapi/token" "OPENAI_API_KEY"))

  (let ((chatanywhere-token (get-api-token "chatanywhere/token" "CHATANYWHERE_IP_KEY")))
    (defvar gptel--chatanywhere
      (gptel-make-openai "ChatAnyWhere"
        :host "api.chatanywhere.tech"
        :endpoint "/v1/chat/completions"
        :models '("gpt-4" "gpt-4o" "gpt-4o-mini")
        :key chatanywhere-token
        ;; :header `(("Authorization" . ,(format "Bearer %s" chatanywhere-token)))
        :stream t)))

  (let ((gemini-token (get-api-token "gemini/token" "GEMINI_IP_KEY")))
    (defvar gptel--gemini (gptel-make-gemini "Gemini" :key gemini-token :stream t)))

  ;; TODO 定义其他大模型后端 ...

  ;; 默认后端
  (setq-default gptel-backend gptel--chatanywhere)

  (global-set-key (kbd "\C-ca") 'gptel)
  (map! :leader
        (:prefix ("ya" . "AI")
         :desc "gptel" :nv "a" #'gptel
         :desc "gptel menu" :nv "m" #'gptel-menu))
  )

(provide 'ai)

;;; ai.el ends here.
