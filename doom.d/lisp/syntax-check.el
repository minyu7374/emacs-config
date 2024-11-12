;;; syntax-check.el -- Syntax Check Config
;;; Commentary:
;;      语法检查设置

;;; Code:
;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (setq lsp-diagnostics-provider :none) ;; don't use 'lsp as default flycheck checker, this will turn off the flycheck auto-start too.
;;                          (setq flycheck-checker 'python-pyright) ;; use 'python-pyright instead as python checker.
;;                          (global-flycheck-mode) ;; enable flycheck manually.
;;                          (require 'lsp-pyright) ;; use lsp-pyright lsp
;;                          (lsp-deferred)

;; (defun use-python-pyright-checker ()
;;   "Set flycheck to use python-pyright checker for Python files."
;;   (setq-local flycheck-checker 'python-pyright)         ;; 强制使用 python-pyright
;;   ;; (flycheck-disable-checker 'lsp)                       ;; 禁用 lsp checker
;;   )

;; (add-hook 'python-mode-hook #'use-python-pyright-checker)

(provide 'syntax-check)

;;; syntax-check.el ends here.
