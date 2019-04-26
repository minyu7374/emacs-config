;; 中英文字符间加空格在org-mode真实写入
(add-hook 'org-mode-hook
          '(lambda ()
             (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; org-mode自动折行
;; (add-hook 'org-mode-hook (lambda () (setq toggle-truncate-lines t)))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; flycheck shellcheck
(add-hook 'sh-mode-hook 'flycheck-mode)

(provide 'hook)
