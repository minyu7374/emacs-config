;;; ai-claude.el --- Claude Code conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/stevemolitor/claude-code.el
;; 从 claudemacs 切回：claude-code 功能更完整（CLI hooks、plan/auto-accept 模式循环、fork 等）。
;; claudemacs 的多 agent 曾是卖点，但 codex 实测体验差，多端优势不成立，故只集成 claude。
(use-package! claude-code
  :commands (claude-code claude-code-resume claude-code-continue claude-code-transient)   ;; 运行命令才加载
  :bind-keymap ("\C-cc" . claude-code-command-map)   ;; C-c c 进入 claude-code 命令前缀
  :config
  ;; 后端保持默认 eat，实际使用体验优于 vterm
  ;; (setq claude-code-terminal-backend 'vterm)
  (claude-code-mode))

;; 供应商切换由外部 cc-switch 管理，Emacs 内不再做后端管控逻辑。

;; C-c c 原为 comment-line，这里覆盖；平时用 SPC c SPC 注释，不冲突。
(map! :leader
      (:prefix ("yc" . "Claude Code")
       :desc "Start Claude"            :nv "c" #'claude-code
       :desc "Resume session"          :nv "r" #'claude-code-resume
       :desc "Continue session"        :nv "C" #'claude-code-continue
       :desc "Toggle window"           :nv "t" #'claude-code-toggle
       :desc "Send region/buffer"      :nv "s" #'claude-code-send-region
       :desc "Fix error at point"      :nv "e" #'claude-code-fix-error-at-point
       :desc "Kill session"            :nv "q" #'claude-code-kill
       :desc "Transient menu"          :nv "m" #'claude-code-transient))

(provide 'ai-claude)

;;; ai-claude.el ends here.
