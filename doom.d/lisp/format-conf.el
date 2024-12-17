;;; format-conf.el -- Format Config
;;; Commentary:
;;      代码格式化相关配置

;;; Code:

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; 对于特定的模式，如 C/C++，可以单独设置
(after! c-mode
  (setq-default c-basic-offset 4))
(after! c++-mode
  (setq-default c-basic-offset 4))

;; 默认black太严格了，有些风格也不是很喜欢，可用autopep8/yapf/flake8/pylint等
(setq-hook! 'python-mode-hook +format-with 'yapf)

;; 检查环境变量以禁用自动格式化，可与direnv结合从而控制不同项目目录下的行为
(defun disable-format-on-save-by-env ()
  "Disable format-on-save if FORMAT_ON_SAVE environment variable is set to 'false'."
  (when (string= (getenv "FORMAT_ON_SAVE") "false")
    (setq +format-with-lsp nil)  ;; 禁用 LSP 的格式化功能
    (setq +format-inhibit t)))   ;; 禁用 Doom Emacs 的格式化功能

(add-hook! 'python-mode-hook 'disable-format-on-save-by-env)
(add-hook! 'c-mode-hook 'disable-format-on-save-by-env)
(add-hook! 'c++-mode-hook 'disable-format-on-save-by-env)
(add-hook! 'java-mode-hook 'disable-format-on-save-by-env)

(provide 'format-conf)

;;; format-conf.el ends here.
