;;; base.el --- Base Config
;;; Commentary:
;;      基础配置

;;; code:

;; 兼容大写UTF-8的编码声明（标准应为小写）
(define-coding-system-alias 'UTF-8 'utf-8)

(global-goto-address-mode t)

(setq mouse-drag-copy-region t)

(provide 'base)

;;; base.el ends here.
