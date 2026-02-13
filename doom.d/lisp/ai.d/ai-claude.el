;;; ai-claude.el --- Claude Code conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/stevemolitor/claude-code.el
(use-package! claude-code
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
            `((ccr . ,(lambda ()
                        (setenv "ANTHROPIC_BASE_URL" (format "%s://%s" (+llm-get-provider-protocol 'ccr) (+llm-get-provider-conf 'ccr :host)))
                        (setenv "ANTHROPIC_AUTH_TOKEN" (+llm-get-provider-conf 'ccr :token))))
              (zai . ,(lambda ()
                        ;; (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL": "glm-4.5-air")
                        ;; (setenv "ANTHROPIC_DEFAULT_SONNET_MODEL": "glm-4.7")
                        ;; (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL": "glm-4.7")
                        (setenv "ANTHROPIC_BASE_URL" (format "%s://%s" (+llm-get-provider-protocol 'zai) (+llm-get-provider-conf 'zai :claude-host)))
                        (setenv "ANTHROPIC_AUTH_TOKEN" (+llm-get-provider-conf 'zai :token))))))
      (+claude--switch-backend (or backend +claude--active-backend (caar +claude--backends)))
      (setq +claude--backend-setup-done t)))

  (defun +claude--switch-backend (backend)
    "Switch claude-code backend to BACKEND."
    (interactive
     (list (intern (completing-read "Select backend: " (mapcar #'car +claude--backends) nil t))))
    (setq +claude--active-backend backend)
    (let ((func (alist-get +claude--active-backend +claude--backends)))
      (when func
        (funcall func)
        (message "Claude backend success switched to: %s" +claude--active-backend))))

  :config
  (+claude--backend-setup 'ccr)

  (setq claude-code-terminal-backend 'eat) ;; vterm/eat

  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)

  ;; :bind-keymap
  ;; ("C-c c" . claude-code-command-map) ;; C-c c 原本绑定comment-line，但平时都用 SPC c SPC，这里覆盖掉也没事

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(global-set-key (kbd "\C-cc") 'claude-code-command-map) ;; C-c c 原本绑定comment-line，但平时都用 SPC c SPC，这里覆盖掉也没事
(map! :leader
      (:prefix ("yc" . "Claude Code")
       :desc "Claude Code"                 :nv "c" #'claude-code
       :desc "Kill Claude Code"            :nv "k" #'claude-code-kill
       :desc "Kill All Claude Code"        :nv "K" #'claude-code-kill-all
       :desc "Claude backend setup"        :nv "b" #'+claude--backend-setup
       :desc "Switch Claude backend"       :nv "s" #'+claude--switch-backend
       :desc "Claude Code transient menu"  :nv "m" #'claude-code-transient
       :desc "Claude Code slash commands"  :nv "/" #'claude-code-slash-commands))

(provide 'ai-claude)

;;; ai-claude.el ends here.
