;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(defun get-llm-api-host (env-var default-host)
  "Retrieve the llm api host from an environment variable.
ENV-VAR is the name of the environment variable.
DEFAULT-HOST is the fallback value if ENV-VAR is not set or empty."
  (let ((host (getenv env-var)))
    (if (or (not host) (string-empty-p host)) default-host host)))

(defun get-llm-api-token (pass-var env-var)
  "Retrieve the llm api token from a password manager or an environment variable.
PASS-VAR is the key for the token stored in GNU Pass.
ENV-VAR is the environment variable to use as a fallback if PASS-VAR is empty."
  (let ((token (string-trim (shell-command-to-string (format "pass %s" pass-var)))))
    (if (or (not token) (string-empty-p token))
        (getenv env-var) token)))

(defvar llm-api-keys
  `((openai
     :token ,(lambda () (get-llm-api-token "openai/token" "OPENAI_API_KEY")))
    (chatanywhere
     :host ,(lambda () (get-llm-api-host "CHATANYWHERE_API_HOST" "api.chatanywhere.tech"))
     :token ,(lambda () (get-llm-api-token "chatanywhere/token" "CHATANYWHERE_API_KEY")))
    (gemini
     :token ,(lambda () (get-llm-api-token "gemini/token" "GEMINI_API_KEY")))
    (minyuchat
     :host ,(lambda () (get-llm-api-host "MINYUCHAT_API_HOST" "chat.wminyu.top:433"))
     :ds-host ,(lambda () (get-llm-api-host "MINYUCHAT_DS_API_HOST" "chat.wminyu.top:433/v1"))
     :token ,(lambda () (get-llm-api-token "deepseek/web-token" "DEEPSEEK_WEB_API_KEY")))))

(defvar llm-api-keys-cache (make-hash-table :test 'equal)
  "Cache for storing already loaded llm api keys.")

(defun get-llm-api-key (backend key)
  "Retrieve property value for a llm api, caching the result after first load.
BACKEND is the name of the llm(e.g. gemini, chatanywhere, ...).
KEY is the property to retrieve (:*host or :token)."
  (let ((cache-key (cons backend key)))
    (or (gethash cache-key llm-api-keys-cache)
        (let* ((entry (alist-get backend llm-api-keys))
               (val (plist-get entry key))
               (result (when val (funcall val))))
          (when result
            (puthash cache-key result llm-api-keys-cache))
          result))))

;; gptel https://github.com/karthink/gptel
(use-package! gptel
  :config
  (setq gptel-max-tokens 4096)
  (setq gptel-track-media t)

  ;; (setq gptel-api-key (get-llm-api-key 'openai :token))

  (defvar gptel--gemini (gptel-make-gemini "Gemini" :key (get-llm-api-key 'gemini :token) :stream t))

  (defvar gptel--chatanywhere
    (gptel-make-openai "ChatAnyWhere"
      :host (get-llm-api-key 'chatanywhere :host)
      :key (get-llm-api-key 'chatanywhere :token)
      ;; :header `(("Authorization" . ,(concat "Bearer " chatanywhere-token)))
      ;; :endpoint "/v1/chat/completions"
      :stream t
      :models '(gpt-4 gpt-4o gpt-4o-mini)))

  ;; deepseek web页面逆向api部署本地，nginx反向代理
  (defvar gptel--minyuchat
    (gptel-make-openai "MinyuChat"
      :host (get-llm-api-key 'minyuchat :host)
      :key (get-llm-api-key 'minyuchat :token)
      :stream t
      :models '(deepseek-chat deepseek-coder deepseek-reasoner)))

  ;; 默认后端
  (setq-default gptel-backend gptel--minyuchat)

  (global-set-key (kbd "\C-cA") 'gptel)
  (map! :leader
        (:prefix ("ya" . "gptel")
         :desc "Create new chat buffer" :nv "a" #'gptel
         :desc "Menu for chat preferences" :nv "m" #'gptel-menu
         :desc "Rewrite/refactor selected region" :nv "w" #'gptel-rewrite
         :desc "Add/remove region/buffer to chat context" :nv "d" #'gptel-add
         :desc "Add a file to chat context" :nv "f" #'gptel-add-file
         :desc "Stop active gptel process" :nv "k" #'gptel-abort))
  )

;; chatgpt-shell https://github.com/xenodium/chatgpt-shell
(use-package! chatgpt-shell
  :config
  (setq chatgpt-shell-api-url-base             (format "https://%s" (get-llm-api-key 'chatanywhere :host))
        chatgpt-shell-openai-key               (get-llm-api-key 'chatanywhere :token)
        chatgpt-shell-google-key               (get-llm-api-key 'gemini :token)
        chatgpt-shell-deepseek-api-url-base    (format "https://%s" (get-llm-api-key 'minyuchat :ds-host))
        chatgpt-shell-deepseek-key             (get-llm-api-key 'minyuchat :token))

  (global-set-key (kbd "\C-cC") 'chatgpt-shell)
  (global-set-key (kbd "\C-ce") 'chatgpt-shell-prompt-compose)
  (map! :leader
        (:prefix ("yc" . "chatgpt-shell")
         :desc "Start new Chatgpt-Shell interactive" :nv "c" #'chatgpt-shell
         :desc "Compose and send prompt from a dedicated buffer" :nv "e" #'chatgpt-shell-prompt-compose
         :desc "Cancel and close compose buffer" :nv "q" #'chatgpt-shell-prompt-compose-cancel
         :desc "Chatgpt-Shell swap model" :nv "v" #'chatgpt-shell-swap-model
         :desc "Chatgpt-Shell swap prompt" :nv "s" #'chatgpt-shell-swap-system-prompt))
  )

;; copilot https://github.com/copilot-emacs/copilot.el
;; accept completion from copilot and fallback to company
(use-package! copilot
  ;; :hook (prog-mode . copilot-mode) ;; 先不默认启动了
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-/" . 'copilot-accept-completion)
              ("S-<tab>" . 'copilot-accept-completion-by-word)
              ("S-TAB" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config

  ;; 添加开关
  (global-set-key (kbd "\C-cG") #'copilot-mode)
  (map! :leader
        :desc "Toggle Copilot mode" "t G" #'copilot-mode)

  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )

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
    (kbd "C-p") (lambda () (interactive) (my/copilot-action-or-default #'copilot-previous-completion)))
  )

(provide 'ai)

;;; ai.el ends here.
