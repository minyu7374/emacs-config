;;; ai-claude.el --- Claude Code conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/stevemolitor/claude-code.el
(use-package! claude-code
  :config 
  (setenv "ANTHROPIC_BASE_URL" (replace-regexp-in-string "{{EP_ID}}" (get-llm-api-key 'wq :ep) (get-llm-api-key 'wq :host)))
  (setenv "ANTHROPIC_AUTH_TOKEN" (get-llm-api-key 'wq :token))

  ;; optional IDE integration with Monet
  ;; (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  ;; (monet-mode 1)
  (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode)

  ;; :bind-keymap
  ;; ("C-c c" . claude-code-command-map) ;; C-c c 原本绑定comment-line，但平时都用 SPC c SPC，这里覆盖掉也没事

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(global-set-key (kbd "\C-cc") 'claude-code-command-map) ;; C-c c 原本绑定comment-line，但平时都用 SPC c SPC，这里覆盖掉也没事
(map! :leader
      (:prefix ("yc" . "Claude Code")
       :desc "Claude Code" :nv "c" #'claude-code
       :desc "Claude Code transient menu" :nv "m" #'claude-code-transient
       :desc "Claude Code slash commands" :nv "/" #'claude-code-slash-commands))

(provide 'ai-claude)

;;; ai-claude.el ends here.
