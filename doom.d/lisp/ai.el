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

;; key 不大可能变动，不常需要，就不设置快捷键了
(defun clear-llm-cache ()
  "Clear the LLM API keys cache. Useful when API keys are updated."
  (interactive)
  (clrhash +llm-api-keys-cache)
  (setq +gptel--backend-setup-done nil
        +aider--backend-setup-done nil)
  (message "LLM cache cleared, backends will be reinitialized"))

;; gptel https://github.com/karthink/gptel
;; 用tools/llm, 改为 after!
(after! gptel
  (setq gptel-max-tokens 8192)
  (setq gptel-track-media t)

  ;;随便设置一个值，避免执行gptel时选用和api-key判空
  (setq gptel-backend (gptel-make-openai " By SetUp"))
  (setq gptel-api-key "sk-null")

  (defvar +gptel--backends nil "gptel backends can use.")
  (defvar +gptel--backend-setup-done nil)

  (defun +gptel--backend-setup ()
    "Setup backend for gptel."
    (interactive)
    (unless +gptel--backend-setup-done
      ;; (setq gptel-api-key (get-llm-api-key 'openai :token))
      (setq +gptel--backends
            `((gemini . ,(gptel-make-gemini "Gemini" :key (get-llm-api-key 'gemini :token) :stream t))

              (chatanywhere . ,(gptel-make-openai "ChatAnyWhere"
                                 :host (get-llm-api-key 'chatanywhere :host)
                                 :key (get-llm-api-key 'chatanywhere :token)
                                 ;; :header `(("Authorization" . ,(concat "Bearer " chatanywhere-token)))
                                 ;; :endpoint "/v1/chat/completions"
                                 :stream t
                                 :models '(gpt-4 gpt-4o gpt-4o-mini)))

              ;; deepseek web页面逆向api部署本地，nginx反向代理
              (minyuchat . ,(gptel-make-openai "MinyuChat"
                              :host (get-llm-api-key 'minyuchat :host)
                              :key (get-llm-api-key 'minyuchat :token)
                              :stream t
                              :models '(deepseek-chat deepseek-coder deepseek-reasoner)))))

      ;; 默认后端
      (setq gptel-backend (alist-get 'gemini +gptel--backends))
      (setq gptel-model 'gemini-2.5-flash)
      (message "gptel backend set to: %s" (gptel-backend-name gptel-backend))
      (setq +gptel--backend-setup-done t)))

  (defun +gptel--ensure-backend (&rest _args) 
    (+gptel--backend-setup))

  (advice-add  #'gptel :before #'+gptel--ensure-backend) ;; 明确指定gptel函数引用
  (advice-add 'gptel-menu :before #'+gptel--ensure-backend)
  (advice-add 'gptel-rewrite :before #'+gptel--ensure-backend))

(global-set-key (kbd "\C-cg") 'gptel)
(map! :leader
      (:prefix ("yg" . "gptel")
       :desc "gptel backend setup"         :nv "b" #'+gptel--backend-setup
       :desc "Create new chat buffer"      :nv "g" #'gptel
       :desc "Menu for chat preferences"   :nv "m" #'gptel-menu
       :desc "Rewrite/refactor selected region"         :nv "w" #'gptel-rewrite
       :desc "Add/remove region/buffer to chat context" :nv "d" #'gptel-add
       :desc "Add a file to chat context"  :nv "f" #'gptel-add-file
       :desc "Stop active gptel process"   :nv "k" #'gptel-abort))

;; copilot https://github.com/copilot-emacs/copilot.el
;; accept completion from copilot and fallback to company
(use-package! copilot
  :defer t
  ;; :hook (prog-mode . copilot-mode) ;; 先不默认启动了
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("S-<tab>" . 'copilot-accept-completion-by-word)
              ("S-TAB" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config

  ;; 添加开关
  (global-set-key (kbd "\C-cC") #'copilot-mode)
  (map! :leader
        :desc "Toggle Copilot mode" "t C" #'copilot-mode)

  ;; (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  ;; (add-to-list 'copilot-indentation-alist '(org-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(text-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )

;; (after! (evil copilot)
;;   ;; Define a custom function for handling Copilot or fallback actions
;;   (defun my/copilot-action (copilot-action fallback-action)
;;     "Perform COPILOT-ACTION if copilot-mode is active, otherwise do the default action."
;;     (if (and (bound-and-true-p copilot-mode)
;;              (copilot--overlay-visible)) ; Ensure a completion is visible
;;         (funcall copilot-action)
;;       (funcall fallback-action)))

;;   ;; Bind keys in Evil insert state globally
;;   (evil-define-key 'insert 'global
;;     (kbd "<tab>") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion #'indent-for-tab-command))
;;     (kbd "TAB") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion #'indent-for-tab-command))
;;     (kbd "S-<tab>") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion-by-word #'indent-for-tab-command))
;;     (kbd "S-TAB") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion-by-word #'indent-for-tab-command))
;;     (kbd "C-n") (lambda () (interactive) (my/copilot-action #'copilot-next-completion #'evil-paste-pop-next))
;;     (kbd "C-p") (lambda () (interactive) (my/copilot-action #'copilot-previous-completion #'evil-paste-pop)))
;;   )

;; https://github.com/tninja/aider.el
(use-package! aider
  :defer t
  :after transient  ;; 确保 transient 加载完成后再加载 aider
  :commands (aider-run-aider aider-transient-menu)   ;; 运行命令才加载
  :init
  (setenv "AIDER_AUTO_COMMITS" "False") ;; 限制自动提交

  (defvar +aider--backends nil "Aider backends can use.")
  (defvar +aider--active-backend nil "The active Aider backend.")
  (defvar +aider--backend-setup-done nil)

  (defun +aider--backend-setup ()
    "Setup backend for Aider."
    (interactive)
    (unless +aider--backend-setup-done
      (setq +aider--backends
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
      (setq +aider--active-backend "Gemini")
      (+aider--set-backend +aider--active-backend)
      (setq +aider--backend-setup-done t)))

  (defun +aider--switch-backend ()
    "Interactively switch the Aider backend.
    Prompts the user to select from `+aider--backends` and applies the
    corresponding setup function. Updates `+aider--active-backend`."
    (interactive)
    (let* ((backend-names (mapcar #'car +aider--backends))
           (selected-backend (completing-read "Select Aider active backend: " backend-names nil t +aider--active-backend)))
      (when selected-backend
        (setq +aider--active-backend selected-backend)
        (+aider--set-backend selected-backend)
        (message "Aider active backend switched to: %s" selected-backend))))

  (defun +aider--set-backend (backend)
    (let* ((default-backend-entry (assoc backend +aider--backends))
           (default-setup-fn (plist-get (cdr default-backend-entry) :setup)))
      (when default-setup-fn
        (funcall default-setup-fn)
        (message "Aider backend set to: %s" backend))))

  (defun +aider--ensure-backend (&rest _args)
    (unless +aider--backend-setup-done
      (+aider--backend-setup)))

  (advice-add 'aider-run-aider :before #'+aider--ensure-backend)
  (advice-add 'aider-transient-menu :before #'+aider--ensure-backend)

  :config
  (aider-magit-setup-transients)
  ;; (require 'aider-helm)
  (require 'aider-doom))

(global-set-key (kbd "\C-ca") 'aider-transient-menu) ;; for wider screen
(map! :leader
      (:prefix ("ya" . "aider")
       :desc "Aider backend setup"   :nv "b" #'+aider--backend-setup
       :desc "Switch Aider backend"  :nv "s" #'+aider--switch-backend
       :desc "Run Aider"             :nv "a" #'aider-run-aider
       :desc "Aider transient menu"  :nv "m" #'aider-transient-menu))

(provide 'ai)

;;; ai.el ends here.
