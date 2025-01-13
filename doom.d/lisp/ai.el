;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; gptel
(defun get-api-host (env-var default-host)
  "Retrieve the API host from an environment variable.
ENV-VAR is the name of the environment variable.
DEFAULT-HOST is the fallback value if ENV-VAR is not set or empty."
  (let ((host (getenv env-var)))
    (if (or (not host) (string-empty-p host)) default-host host)))

(defun get-api-token (pass-var env-var)
  "Retrieve the API token from a password manager or an environment variable.
PASS-VAR is the key for the token stored in GNU Pass.
ENV-VAR is the environment variable to use as a fallback if PASS-VAR is empty."
  (let ((token (string-trim (shell-command-to-string (format "pass %s" pass-var)))))
    (if (or (not token) (string-empty-p token))
        (getenv env-var) token)))

(use-package! gptel
  :config
  (setq gptel-max-tokens 4096)
  (setq gptel-track-media t)

  (setq gptel-api-key (get-api-token "openai/token" "OPENAI_API_KEY"))

  (let ((chatanywhere-host (get-api-host "CHATANYWHERE_API_HOST" "api.chatanywhere.tech"))
        (chatanywhere-token (get-api-token "chatanywhere/token" "CHATANYWHERE_API_KEY")))
    (defvar gptel--chatanywhere
      (gptel-make-openai "ChatAnyWhere"
        :host chatanywhere-host
        :key chatanywhere-token
        ;; :header `(("Authorization" . ,(concat "Bearer " chatanywhere-token)))
        ;; :endpoint "/v1/chat/completions"
        :stream t
        :models '("gpt-4" "gpt-4o" "gpt-4o-mini"))))

  (let ((anthropic-token (get-api-token "anthropic/token" "ANTHROPIC_API_KEY")))
    (defvar gptel--anthropic (gptel-make-anthropic "Claude" :key anthropic-token :stream t)))

  (let ((gemini-token (get-api-token "gemini/token" "GEMINI_API_KEY")))
    (defvar gptel--gemini (gptel-make-gemini "Gemini" :key gemini-token :stream t)))

  ;; Gentoo-GMK 本地部署，直接(或ssh端口转发)进行请求。
  (defvar gptel--gpt4all
    (gptel-make-gpt4all "GPT4All"
      :protocol "http"
      :host "localhost:4891"
      :stream nil
      :models '(qwen2.5-coder-7b-instruct-q4_0.gguf
                Meta-Llama-3-8B-Instruct-Q4_0.gguf
                Llama-3.2-3B-Instruct-Q4_0.gguf)))

  ;; 通过nginx反向代理转发，nginx中做简单的authorization
  (let ((minyuchat-host (get-api-host "MINYUCHAT_API_HOST" "chat.wminyu.top:433"))
        (minyuchat-token (get-api-token "minyuchat/token" "MINYUCHAT_API_KEY")))
    (defvar gptel--minyuchat
      (gptel-make-gpt4all "Minyu"
        :host minyuchat-host
        :key minyuchat-token
        :stream nil
        :models '(qwen2.5-coder-7b-instruct-q4_0.gguf
                  Meta-Llama-3-8B-Instruct-Q4_0.gguf
                  Llama-3.2-3B-Instruct-Q4_0.gguf))))

  ;; 默认后端
  (setq-default gptel-backend gptel--chatanywhere)

  (global-set-key (kbd "\C-ca") 'gptel)
  (map! :leader
        (:prefix ("ya" . "AI")
         :desc "Create new chat buffer" :nv "a" #'gptel
         :desc "Menu for chat preferences" :nv "m" #'gptel-menu
         :desc "Rewrite/refactor selected region" :nv "w" #'gptel-rewrite
         :desc "Add/remove region/buffer to chat context" :nv "d" #'gptel-add
         :desc "Add a file to chat context" :nv "f" #'gptel-add-file
         :desc "Stop active gptel process" :nv "k" #'gptel-abort))
  )


;; copilot https://github.com/copilot-emacs/copilot.el
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-/" . 'copilot-accept-completion)
              ("S-<tab>" . 'copilot-accept-completion-by-word)
              ("S-TAB" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(after! (evil copilot)
  ;; Define a custom function for handling Copilot or fallback actions
  (defun my/copilot-action-or-default (copilot-action)
    "Perform COPILOT-ACTION if copilot-mode is active, otherwise do the default action."
    (if (and (bound-and-true-p copilot-mode)
             (copilot--overlay-visible)) ; Ensure a completion is visible
        (funcall copilot-action)
      (call-interactively #'self-insert-command))) ; Default to inserting the key's character

  ;; Bind keys in Evil insert state globally
  (evil-define-key 'insert 'global
    (kbd "<tab>") (lambda () (interactive) (my/copilot-action-or-default #'copilot-accept-completion))
    (kbd "TAB") (lambda () (interactive) (my/copilot-action-or-default #'copilot-accept-completion))
    (kbd "C-/") (lambda () (interactive) (my/copilot-action-or-default #'copilot-accept-completion))
    (kbd "S-<tab>") (lambda () (interactive) (my/copilot-action-or-default #'copilot-accept-completion-by-word))
    (kbd "S-TAB") (lambda () (interactive) (my/copilot-action-or-default #'copilot-accept-completion-by-word))
    (kbd "C-n") (lambda () (interactive) (my/copilot-action-or-default #'copilot-next-completion))
    (kbd "C-p") (lambda () (interactive) (my/copilot-action-or-default #'copilot-previous-completion))))

(provide 'ai)

;;; ai.el ends here.
