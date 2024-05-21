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

(setq +format-on-save-disabled-modes
      '(emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        xml-mode nxml-mode
        latex-mode))

(provide 'format-conf)

;;; format-conf.el ends here.
