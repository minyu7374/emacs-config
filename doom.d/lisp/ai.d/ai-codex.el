;;; ai-codex.el --- Codex conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2026  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; https://github.com/dgillis/emacs-codex-ide
;; 纯 Emacs 的 Codex 客户端（claude-code-ide 的 Codex 对应物）：对接 codex app-server，
;; 无终端包装——会话就是普通 buffer，代码块按 major-mode 高亮，diff 走 diff-mode，
;; 命令/改动审批在 buffer 内交互完成，无需 eat/vterm。
;; 供应商切换由外部 cc-switch 管理，Emacs 内不再做后端管控逻辑。
(use-package! codex-ide
  :commands (codex-ide codex-ide-menu codex-ide-continue codex-ide-prompt)
  :bind ("\C-cx" . codex-ide-menu)
  :config
  ;; Emacs MCP 桥（对应 claude-code-ide-emacs-tools-setup）：把 buffer/选区/窗口等
  ;; Emacs 状态暴露给 Codex，依赖 python3 + emacsclient（会自动确保 server 运行）。
  ;; 默认值 'prompt 是首次启动会话时询问，这里直接启用。
  (setq codex-ide-want-mcp-bridge t))

(map! :leader
      (:prefix ("yx" . "Codex IDE")
       :desc "Start Codex"             :nv "c" #'codex-ide
       :desc "Continue session"        :nv "C" #'codex-ide-continue
       :desc "Show session buffer"     :nv "t" #'codex-ide-switch-to-buffer
       :desc "Send prompt"             :nv "s" #'codex-ide-prompt
       :desc "Session diff"            :nv "d" #'codex-ide-session-diff-open
       :desc "List sessions"           :nv "l" #'codex-ide-session-buffer-list
       :desc "Interrupt turn"          :nv "i" #'codex-ide-interrupt
       :desc "Stop session"            :nv "q" #'codex-ide-stop
       :desc "Menu"                    :nv "m" #'codex-ide-menu))

(provide 'ai-codex)

;;; ai-codex.el ends here.
