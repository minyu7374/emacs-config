;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-
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
  (setq-default gptel-backend gptel--gemini)

  (global-set-key (kbd "\C-cg") 'gptel)
  (map! :leader
        (:prefix ("yg" . "gptel")
         :desc "Create new chat buffer" :nv "b" #'gptel
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

  (global-set-key (kbd "\C-cG") 'chatgpt-shell)
  (global-set-key (kbd "\C-cE") 'chatgpt-shell-prompt-compose)
  (map! :leader
        (:prefix ("yG" . "chatgpt-shell")
         :desc "Start new Chatgpt-Shell interactive" :nv "i" #'chatgpt-shell
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
  (global-set-key (kbd "\C-cC") #'copilot-mode)
  (map! :leader
        :desc "Toggle Copilot mode" "t C" #'copilot-mode)

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


;; https://github.com/tninja/aider.el
(use-package! aider
  :after transient  ;; 确保 transient 加载完成后再加载 aider
  :commands (aider-run-aider)   ;; 运行命令才加载
  :init
  (setenv "AIDER_AUTO_COMMITS" "False") ;; 限制自动提交
  (defvar +aider-backends
    '(("Gemini"
       :setup (lambda ()
                ;; (setq aider-args `("--api-key" ,(format "gemini=%s" (get-llm-api-key 'gemini :token))
                ;;                    "--model" "gemini/gemini-2.5-flash"))))
                (setenv "GEMINI_API_KEY" (get-llm-api-key 'gemini :token))
                (setq aider-args `("--model" "gemini/gemini-2.5-flash"))))
      ("MinyuChat"
       :setup (lambda ()
                (setenv "DEEPSEEK_API_BASE" (format "https://%s" (get-llm-api-key 'minyuchat :ds-host)))
                (setenv "DEEPSEEK_API_KEY" (get-llm-api-key 'minyuchat :token))
                (setq aider-args `("--model" "deepseek"))))))
  (defvar +aider-active-backend "Gemini"
    "The active Aider backend.")

  (defun aider-switch-backend ()
    "Interactively switch the Aider backend.
    Prompts the user to select from `+aider-backends` and applies the
    corresponding setup function. Updates `+aider-active-backend`."
    (interactive)
    (let* ((backend-names (mapcar #'car +aider-backends))
           (selected-backend-name (completing-read "Select Aider backend: " backend-names nil t +aider-active-backend))
           (backend-entry (assoc selected-backend-name +aider-backends))
           (setup-fn (plist-get (cdr backend-entry) :setup)))
      (when setup-fn
        (funcall setup-fn)
        (setq +aider-active-backend selected-backend-name)
        (message "Aider backend switched to: %s" +aider-active-backend))))

  :config
  ;; Set initial backend without prompting
  (let* ((default-backend-entry (assoc +aider-active-backend +aider-backends))
         (default-setup-fn (plist-get (cdr default-backend-entry) :setup)))
    (when default-setup-fn
      (funcall default-setup-fn)
      (message "Aider initial backend set to: %s" +aider-active-backend)))

  (aider-magit-setup-transients)
  ;; (require 'aider-helm)
  (require 'aider-doom)
  )

(global-set-key (kbd "\C-ca") 'aider-transient-menu) ;; for wider screen
(map! :leader
      (:prefix ("ya" . "aider")
       :desc "Aider transient menu" :nv "m" #'aider-transient-menu
       :desc "Switch Aider backend" :nv "s" #'aider-switch-backend))

(provide 'ai)

;;; ai.el ends here.
