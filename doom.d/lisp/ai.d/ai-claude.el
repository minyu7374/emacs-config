;;; ai-claude.el --- Claude Code conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/manzaltu/claude-code-ide.el
;; 基于 MCP 的 IDE 集成：Claude 改动走 ediff 审查，诊断/选区自动共享，并把 Emacs 的
;; xref/tree-sitter/imenu/project 暴露成工具给它调用。
;; 供应商切换由外部 cc-switch 管理，Emacs 内不再做后端管控逻辑。
(use-package! claude-code-ide
  :commands (claude-code-ide claude-code-ide-menu claude-code-ide-resume claude-code-ide-continue)   ;; 运行命令才加载
  :bind ("\C-cc" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat)   ;; 默认 vterm
  (claude-code-ide-emacs-tools-setup))            ;; 启用 xref/tree-sitter/imenu/project 等 MCP 工具

(map! :leader
      (:prefix ("yc" . "Claude Code IDE")
       :desc "Start Claude"            :nv "c" #'claude-code-ide
       :desc "Resume session"          :nv "r" #'claude-code-ide-resume
       :desc "Continue session"        :nv "C" #'claude-code-ide-continue
       :desc "Toggle window"           :nv "t" #'claude-code-ide-toggle
       :desc "Send prompt"             :nv "s" #'claude-code-ide-send-prompt
       :desc "Send region as @"        :nv "@" #'claude-code-ide-insert-at-mentioned
       :desc "List sessions"           :nv "l" #'claude-code-ide-list-sessions
       :desc "Stop session"            :nv "q" #'claude-code-ide-stop
       :desc "Menu"                    :nv "m" #'claude-code-ide-menu))

(provide 'ai-claude)

;;; ai-claude.el ends here.
