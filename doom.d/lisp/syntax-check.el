;;; syntax-check.el -- Syntax Check Config  -*- lexical-binding: t; -*-
;;; Commentary:
;;      语法检查设置（flymake，配合 init.el 的 (syntax +childframe +flymake)）。
;;      - 所有 backend 挂在 flymake-diagnostic-functions 上并行跑，eglot 管理的
;;        buffer 自动注册 LSP 诊断；按需禁用某个 backend 用
;;        flymake-disabled-backends 或直接改 diagnostic-functions。
;;      - 弹窗由 Doom 的 flymake-popon（+childframe → posframe）负责，无需配置。
;;      - Doom 已把 next-error-function 接到 flymake，]e / [e 直接可用。

;;; Code:

(after! flymake
  ;; 改动后 idle 多久重查（默认 0.5s 稍显激进；保存时的重查由
  ;; flymake-start-on-save-buffer 默认负责，无需额外配置）。
  (setq flymake-no-changes-timeout 1.0)

  (map! :leader
        (:prefix ("e" . "errors")
         :desc "List errors (buffer)"   "l" #'flymake-show-buffer-diagnostics
         :desc "List errors (project)"  "L" #'flymake-show-project-diagnostics
         :desc "Previous error"         "p" #'flymake-goto-prev-error
         :desc "Next error"             "n" #'flymake-goto-next-error
         :desc "Recheck buffer"         "s" #'flymake-start
         :desc "Toggle flymake"         "t" #'flymake-mode
         :desc "Show log buffer"        "v" #'flymake-switch-to-log-buffer)))

(provide 'syntax-check)

;;; syntax-check.el ends here.
