;;; base.el --- Base Config
;;; Commentary:
;;      基础配置

;;; code:

;; 兼容大写UTF-8的编码声明（标准应为小写）
(define-coding-system-alias 'UTF-8 'utf-8)

(defun base-global-setting()
  (global-goto-address-mode t)
  (+global-word-wrap-mode +1))

(add-hook 'emacs-startup-hook
          (lambda () (run-with-idle-timer 0.5 nil #'base-global-setting)))

(setq mouse-drag-copy-region t)

(setq-default fill-column 120
              delete-trailing-lines t)

(provide 'base)

;;; base.el ends here.
