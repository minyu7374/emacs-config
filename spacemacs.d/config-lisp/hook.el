;; ;; 中英文字符间加空格在org-mode真实写入
;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

;; org-mode自动折行
;; (add-hook 'org-mode-hook (lambda () (setq toggle-truncate-lines t)))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; flycheck
(add-hook 'sh-mode-hook 'flycheck-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)

;; zeal
(add-hook 'python-mode-hook
          (lambda () (setq zeal-at-point-docset '("python" "django"))))

;; 让 Emacs 启动时自动加载 pyim 词库
(add-hook 'emacs-startup-hook
          #'(lambda () (pyim-restart-1 t)))

;; icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Bind clang-format-buffer to tab on the c++-mode only:
(add-hook 'c++-mode-hook 'clang-format-bindings)
(defun clang-format-bindings ()
  (define-key c++-mode-map [tab] 'clang-format-buffer))

;; gtpl
(defun go-web-template-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.gtpl\\'" buffer-file-name))
    (web-mode)
    (web-mode-set-engine 'go)))

(add-hook 'find-file-hook 'go-web-template-mode)

(provide 'hook)
