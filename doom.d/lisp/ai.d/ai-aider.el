;;; ai-aider.el --- Aider conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/MatthewZMD/aidermacs
(use-package! aidermacs
  :commands (aidermacs-run aidermacs-transient-menu)   ;; 运行命令才加载
  :init
  (defvar +aider--backends nil "Aider backends list.")
  (defvar +aider--active-backend nil "Current Active Aider backend.")
  (defvar +aider--backend-setup-done nil)
  ;; backends里设置的是lambda函数，不存储数据。llm-cache清理，也不用重置，无需注册
  ;; (add-hook '+llm-cache-clearers (lambda () (setq +aider--backend-setup-done nil)) 'append)

  (defun +aider--backend-setup (&optional backend)
    "Setup backend for aidermacs."
    (interactive)
    (unless +aider--backend-setup-done
      ;; 一定要修改下 aidermacs-default-model 的设置，不然会被早早地设置成sonet
      (setq +aider--backends
            `((gemini . ,(lambda ()
                           (setenv "GEMINI_API_KEY" (+llm-get-provider-conf 'gemini :token))
                           ;; (setenv "AIDER_MODEL" "gemini/gemini-3-flash-preview")
                           ;; (setenv "AIDER_WEAK_MODEL" "gemini/gemini-2.5-flash")
                           (setq aidermacs-default-model "gemini/gemini-3-flash-preview")
                           (setq aidermacs-weak-model "gemini/gemini-2.5-flash")))

              (nvidia . ,(lambda ()
                           (setenv "OPENAI_API_BASE" (format "https://%s/v1" (+llm-get-provider-conf 'nvidia :host)))
                           (setenv "OPENAI_API_KEY" (+llm-get-provider-conf 'nvidia :token))
                           ;; (setenv "AIDER_MODEL" "openai/z-ai/glm4.7")
                           ;; (setenv "AIDER_WEAK_MODEL" "openai/minimaxai/minimax-m2.1")
                           (setq aidermacs-default-model "openai/z-ai/glm4.7")
                           (setq aidermacs-weak-model "openai/minimaxai/minimax-m2.1")))

              (zai . ,(lambda ()
                        (setenv "OPENAI_API_BASE" (format "https://%s" (+llm-get-provider-conf 'zai :host)))
                        (setenv "OPENAI_API_KEY" (+llm-get-provider-conf 'zai :token))
                        ;; (setenv "AIDER_MODEL" "openai/glm-4.6v")
                        ;; (setenv "AIDER_WEAK_MODEL" "openai/glm-4.5-air")
                        (setq aidermacs-default-model "openai/glm-4.6v")
                        (setq aidermacs-weak-model "openai/glm-4.5-air")))

              ;; Empty response received from LLM. Check your provider account?
              ;; (minyu-ds . ,(lambda ()
              ;;                (setenv "DEEPSEEK_API_BASE" (format "https://%s/v1" (+llm-get-provider-conf 'minyu-ds :host)))
              ;;                (setenv "DEEPSEEK_API_KEY" (+llm-get-provider-conf 'minyu-ds :token))
              ;;                ;; (setenv "AIDER_MODEL" "deepseek/deepseek-reasoner")
              ;;                ;; (setenv "AIDER_WEAK_MODEL" "deepseek/deepseek-chat")
              ;;                (setq aidermacs-default-model "deepseek/deepseek-reasoner")
              ;;                (setq aidermacs-weak-model "deepseek/deepseek-chat")))

              (minyu-qwen . ,(lambda ()
                               (setenv "OPENAI_API_BASE" (format "https://%s/v1" (+llm-get-provider-conf 'minyu-qwen :host)))
                               (setenv "OPENAI_API_KEY" (+llm-get-provider-conf 'minyu-qwen :token))
                               ;; (setenv "AIDER_MODEL" "openai/qwen-max")
                               ;; (setenv "AIDER_WEAK_MODEL" "openai/qwen-plus")
                               (setq aidermacs-default-model "openai/qwen-max")
                               (setq aidermacs-weak-model "openai/qwen-plus")))))

      (setq +aider--backend-setup-done t))
    (+aider--switch-backend (or backend +aider--active-backend (caar +aider--backends))))

  ;; switch 函数拆分为两份：一份仅设置选中的后端，另一份才执行后端配置，在eat启动时运行，保证环境变量更好地继承
  (defun +aider--switch-backend (backend)
    "Switch aidermacs backend to BACKEND."
    (interactive
     (list (intern (completing-read "Select backend: " (mapcar #'car +aider--backends) nil t))))
    (setq +aider--active-backend backend)
    (message "Aidermacs active backend switch to: %s" +aider--active-backend))

  (defun +aider--set-backend (&rest _)
    (let ((func (alist-get +aider--active-backend +aider--backends)))
      (when func
        (funcall func)
        (message "Aidermacs backend success set to: %s" +aider--active-backend))))

  ;; 实测switch只设置 active-backend的值，单独创建set函数，借助这个hook才执行真正的方案，结果是混乱的
  ;; (add-hook 'aidermacs-before-run-backend-hook #'+aider--set-backend)
  ;; 用advice-add效果更稳定
  (dolist (cmd '(aidermacs-run aidermacs-transient-menu))
    (advice-add cmd :before #'+aider--set-backend))

  :config
  (+aider--backend-setup 'nvidia)

  :custom
  ;; See the Configuration section below
  ;; (aidermacs-default-chat-mode 'architect) ;; code/architect/ask
  (aidermacs-extra-args `("--chat-language" "zh" "--no-show-model-warnings"))

  (aidermacs-auto-commits nil))

(global-set-key (kbd "\C-ca") 'aidermacs-transient-menu)
(map! :leader
      (:prefix ("ya" . "Aidermacs")
       :desc "Aidermacs run"               :nv "a" #'aidermacs-run
       :desc "Aidermacs quit"              :nv "q" #'aidermacs-exit
       :desc "Aidermacs backend setup"     :nv "b" #'+aider--backend-setup
       :desc "Switch Aidermacs backend"    :nv "s" #'+aider--switch-backend
       :desc "Aidermacs transient menu"    :nv "m" #'aidermacs-transient-menu))

(provide 'ai-aider)

;;; ai-aider.el ends here.
