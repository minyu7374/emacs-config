;;; ai-aider.el --- Aider conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

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
                        (setq aider-args `("--model" "gemini/gemini-3-flash-preview"))))

              ("Zai"
               :setup (lambda ()
                        (setenv "OPENAI_API_BASE" (format "https://%s" (get-llm-api-key 'zai :host)))
                        (setenv "OPENAI_API_KEY" (get-llm-api-key 'zai :token))
                        (setq aider-args `("--model" "openai/glm-4.7" "--no-show-model-warnings"))))

              ("MinyuChat"
               :setup (lambda ()
                        (setenv "DEEPSEEK_API_BASE" (format "https://%s" (get-llm-api-key 'minyuchat :ds-host)))
                        (setenv "DEEPSEEK_API_KEY" (get-llm-api-key 'minyuchat :token))
                        (setq aider-args `("--model" "deepseek/deepseek-chat"))))))
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

(provide 'ai-aider)

;;; ai-aider.el ends here.
