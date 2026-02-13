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
                           (setq aidermacs-default-model "gemini/gemini-3-flash-preview")
                           (setenv "GEMINI_API_KEY" (+llm-get-provider-conf 'gemini :token))
                           (setenv "AIDER_MODEL" "gemini/gemini-3-flash-preview")
                           (setenv "AIDER_WEAK_MODEL" "gemini/gemini-2.5-flash")))

              (nvidia . ,(lambda ()
                           (setq aidermacs-default-model "openai/z-ai/glm4.7")
                           (setenv "OPENAI_API_BASE" (format "https://%s/v1" (+llm-get-provider-conf 'nvidia :host)))
                           (setenv "OPENAI_API_KEY" (+llm-get-provider-conf 'nvidia :token))
                           (setenv "AIDER_MODEL" "openai/z-ai/glm4.7")
                           (setenv "AIDER_WEAK_MODEL" "openai/minimaxai/minimax-m2.1")))

              (zai . ,(lambda ()
                        (setq aidermacs-default-model "openai/glm-4.6v")
                        (setenv "OPENAI_API_BASE" (format "https://%s" (+llm-get-provider-conf 'zai :host)))
                        (setenv "OPENAI_API_KEY" (+llm-get-provider-conf 'zai :token))
                        (setenv "AIDER_MODEL" "openai/glm-4.6v")
                        (setenv "AIDER_WEAK_MODEL" "openai/glm-4.5-air")))

              (deepseek . ,(lambda ()
                             (setq aidermacs-default-model "deepseek/deepseek-coder")
                             (setenv "DEEPSEEK_API_BASE" (format "https://%s/v1" (+llm-get-provider-conf 'minyuchat :host)))
                             (setenv "DEEPSEEK_API_KEY" (+llm-get-provider-conf 'minyuchat :token))
                             (setenv "AIDER_MODEL" "deepseek/deepseek-coder")
                             (setenv "AIDER_WEAK_MODEL" "deepseek/deepseek-chat")))))

      (+aider--switch-backend (or backend +aider--active-backend (caar +aider--backends)))
      (setq +aider--backend-setup-done t)))

  (defun +aider--switch-backend (backend)
    "Switch aidermacs backend to BACKEND."
    (interactive
     (list (intern (completing-read "Select backend: " (mapcar #'car +aider--backends) nil t))))
    (setq +aider--active-backend backend)
    (let ((func (alist-get +aider--active-backend +aider--backends)))
      (when func
        (funcall func)
        (message "Aidermacs backend success switched to: %s" +aider--active-backend))))

  ;; 实测switch只设置 active-backend的值，单独创建set函数，借助这个hook才执行真正的方案，结果是混乱的
  ;; (add-hook 'aidermacs-before-run-backend-hook #'+aider--set-backend)

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
