;;; base.el --- Base Config
;;; Commentary:
;;      基础配置

;;; code:

;; 兼容大写UTF-8的编码声明（标准应为小写）
(define-coding-system-alias 'UTF-8 'utf-8)

(global-goto-address-mode t)

(setq mouse-drag-copy-region t)

(after! word-wrap
  (+global-word-wrap-mode +1))

(setq-default fill-column 120
              delete-trailing-lines t)

(if (eq system-type 'darwin)
    (progn
      (defconst os-open-cmd 'open "The open cmd of current OS.")
      (defconst os-fullscreen-type 'maximized "The fullscreen type to use for cueernt OS.")
      )
  (progn
    (defconst os-open-cmd 'xdg-open "The open cmd of current OS.")
    (defconst os-fullscreen-type 'fullboth "The fullscreen type to use for cueernt OS.")
    ))

;; treesit 迁移
(setq treesit-font-lock-level 4)
(setq treesit-language-source-alist
      '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make       . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (markdown-inline   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src"))
        (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml/src"))
        (org        . ("https://github.com/milisims/tree-sitter-org"))
        (haskell    . ("https://github.com/tree-sitter/tree-sitter-haskell"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php        . ("https://github.com/tree-sitter/tree-sitter-php" nil "php/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))

(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
        (go-mode         . go-ts-mode)
        (sh-mode         . bash-ts-mode)
        (python-mode     . python-ts-mode)
        ;; (rust-mode       . rust-ts-mode)
        (java-mode       . java-ts-mode)
        (makefile-mode   . make-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (markdown-mode   . markdown-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (json-mode       . json-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

;; (defun my/copy-hooks-to-ts-modes ()
;;   "把 `major-mode-remap-alist` 的原 mode hook 全部加到对应的 ts-mode hook."
;;   (dolist (pair major-mode-remap-alist)
;;     (let* ((orig (car pair))
;;            (ts   (cdr pair))
;;            (orig-hook (intern (format "%s-hook" orig)))
;;            (ts-hook   (intern (format "%s-hook" ts))))
;;       (when (and (boundp orig-hook)
;;                  (listp (symbol-value orig-hook)))
;;         (dolist (fn (symbol-value orig-hook))
;;           (unless (member fn (symbol-value ts-hook))
;;             (add-hook ts-hook fn)))))))

;; ;; 自动在 Doom 初始化语言模块后执行
;; (add-hook 'doom-after-modules-config-hook #'my/copy-hooks-to-ts-modes)

(defun my/install-all-treesit-languages ()
  "自动安装所有treesit解析器."
  (interactive)
  (dolist (lang-pair treesit-language-source-alist)
    (let ((lang (car lang-pair)))
      (unless (treesit-language-available-p lang)
        (message "Installing Tree-sitter grammar for %s..." lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error (message "Failed to install %s: %s" lang err)))))))

(provide 'base)

;;; base.el ends here.
