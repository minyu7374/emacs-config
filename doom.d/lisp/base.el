;;; base.el --- Base Config
;;; Commentary:
;;      基础配置

;;; code:

;; 兼容大写UTF-8的编码声明（标准应为小写）
(define-coding-system-alias 'UTF-8 'utf-8)

(global-goto-address-mode t)

(setq mouse-drag-copy-region t)

;; lsp-mode: plist`s provide better performance in deserialization and also put less presure than `hash-table`s.
(setenv "LSP_USE_PLISTS" "true")

(if (eq system-type 'darwin)
    (progn
      (defconst os-open-cmd 'open "The open cmd of current OS.")
      (defconst os-fullscreen-type 'maximized "The fullscreen type to use for cueernt OS.")
      )
  (progn
    (defconst os-open-cmd 'xdg-open "The open cmd of current OS.")
    (defconst os-fullscreen-type 'fullboth "The fullscreen type to use for cueernt OS.")
    ))


(provide 'base)

;;; base.el ends here.
