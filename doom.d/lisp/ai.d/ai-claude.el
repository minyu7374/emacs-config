;;; ai-claude.el --- Claude Code conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/cpoile/claudemacs
(use-package! claudemacs
  :commands (claudemacs-start-menu claudemacs-resume-menu claudemacs-transient-menu)   ;; 运行命令才加载
  :init
  (defvar +claude--backends nil "Claude backends list.")
  (defvar +claude--active-backend nil "Current Active Claude backend.")
  (defvar +claude--backend-setup-done nil)
  ;; backends里设置的是lambda函数，不存储数据。llm-cache清理，也不用重置，无需注册
  ;; (add-hook '+llm-cache-clearers (lambda () (setq +claude--backend-setup-done nil)) 'append)

  (defun +claude--backend-setup (&optional backend)
    "Setup backend for claude-code."
    (interactive)
    (unless +claude--backend-setup-done
      (setq +claude--backends
            `((gemini . ,(lambda()
                           ;; 支持gemini
                           (setenv "GEMINI_API_KEY" (+llm-get-provider-conf 'gemini :token))))
              (ccr . ,(lambda ()
                        (setenv "ANTHROPIC_BASE_URL" (format "%s://%s" (+llm-get-provider-protocol 'ccr) (+llm-get-provider-conf 'ccr :host)))
                        (setenv "ANTHROPIC_AUTH_TOKEN" (+llm-get-provider-conf 'ccr :token))))
              (zai . ,(lambda ()
                        ;; (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL": "glm-4.5-air")
                        ;; (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL": "glm-4.7")
                        ;; (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL": "glm-4.7")
                        (setenv "ANTHROPIC_BASE_URL" (format "%s://%s" (+llm-get-provider-protocol 'zai) (+llm-get-provider-conf 'zai :claude-host)))
                        (setenv "ANTHROPIC_AUTH_TOKEN" (+llm-get-provider-conf 'zai :token))))))
      (setq +claude--backend-setup-done t))
    (+claude--switch-backend (or backend +claude--active-backend (caar +claude--backends))))

  ;; switch 函数拆分为两份：一份仅设置选中的后端，另一份才执行后端配置，在eat启动时运行，保证环境变量更好地继承
  (defun +claude--switch-backend (backend)
    "Switch claudemacs backend to BACKEND."
    (interactive
     (list (intern (completing-read "Select backend: " (mapcar #'car +claude--backends) nil t))))
    (setq +claude--active-backend backend)
    (message "Claudemacs active backend switch to: %s" +claude--active-backend))

  (defun +claude--set-backend (&rest _)
    (let ((func (alist-get +claude--active-backend +claude--backends)))
      (when func
        (funcall func)
        (message "Claudemacs backend success set to: %s" +claude--active-backend))))

  (dolist (cmd '(claudemacs-start-menu claudemacs-resume-menu claudemacs-transient-menu))
    (advice-add cmd :before #'+claude--set-backend))

  :config
  (+claude--backend-setup 'ccr))

(global-set-key (kbd "\C-cc") 'claudemacs-transient-menu) ;; C-c c 原本绑定comment-line，但平时都用 SPC c SPC，这里覆盖掉也没事
(map! :leader
      (:prefix ("yc" . "Claudemacs")
       :desc "Claudemacs start"            :nv "c" #'claudemacs-start-menu
       :desc "Claudemacs resume"           :nv "r" #'claudemacs-resume-menu
       :desc "Quit Claudemacs"             :nv "q" #'claudemacs-kill
       :desc "Claudemacs backend setup"    :nv "b" #'+claude--backend-setup
       :desc "Switch Claudemacs backend"   :nv "s" #'+claude--switch-backend
       :desc "Claudemacs transient menu"   :nv "m" #'claudemacs-transient-menu))

(provide 'ai-claude)

;;; ai-claude.el ends here.
