;;; lsp-conf.el -- LSP Config
;;; Commentary:
;;      LSP设置

;;; Code:

;; lsp-sommand-map: <leader>cl
(after! lsp-mode
  ;; ruff类型检查方面弱于pyright，风格检查方面因为有格式化工具也不太需要
  ;; (add-to-list 'lsp-disabled-clients 'ruff)
  (map! :map lsp-command-map "s" #'+lsp/switch-client)
  (which-key-add-keymap-based-replacements lsp-command-map "s" "Switch LSP client"))

;; 明确针对python只使用pyright，避免多端共存引起dual diagnostics
(after! python
  (add-hook 'python-mode-hook
            (lambda () (setq-local lsp-enabled-clients '(pyright)))))

;; 使用basedpyright
(use-package! lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "basedpyright"))

(after! lsp-ui
  ;; 默认关闭lsp-ui-doc-mode，在打开时同时show-with-cursor生效
  (setq lsp-ui-doc-enable nil)
  ;;top/bottom/at-point
  (setq lsp-ui-doc-position 'at-point)
  ;; (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-ui-doc-show-with-mouse nil)

  ;; (setq lsp-ui-imenu-window-width 30)
  ;; (setq lsp-ui-imenu--custom-mode-line-format t)
  ;; (setq lsp-ui-imenu-auto-refresh t)

  ;; 为lsp-ui(-doc)-mode 创建对应的全局minor mode，关闭全局mode后新buffer不再打开对应mode
  (define-globalized-minor-mode global-lsp-ui-mode
    lsp-ui-mode (lambda () (lsp-ui-mode 1)))

  (define-globalized-minor-mode global-lsp-ui-doc-mode
    lsp-ui-doc-mode (lambda () (lsp-ui-doc-mode 1)))

  ;; 默认启用lsp-ui，关闭lsp-ui-doc
  (global-lsp-ui-mode 1)
  (global-lsp-ui-doc-mode -1)
  ;; doomemacs 在加载lsp-ui的时候加上了lsp-mode的hook，想由global-lsp-ui-mode控制需要去除
  (remove-hook 'lsp-mode-hook #'lsp-ui-mode)

  ;; 也可以使用K查看,会新开一个help window
  (map! :desc "Show documentation" :nv "gk" #'lsp-ui-doc-show)

  (global-set-key (kbd "\C-clu") 'lsp-ui-mode)
  (global-set-key (kbd "\C-clk") 'lsp-ui-doc-mode)
  (global-set-key (kbd "\C-clU") #'global-lsp-ui-mode)
  (global-set-key (kbd "\C-clK") #'global-lsp-ui-doc-mode)
  (global-set-key (kbd "\C-cle") 'lsp-ui-flycheck-list)
  (global-set-key (kbd "\C-cli") 'lsp-ui-imenu)

  (map! :map lsp-command-map
        "u" #'lsp-ui-mode
        "k" #'lsp-ui-doc-mode
        "U" #'global-lsp-ui-mode
        "K" #'global-lsp-ui-doc-mode
        "e" #'lsp-ui-flycheck-list
        "i" #'lsp-ui-imenu)
  (which-key-add-keymap-based-replacements lsp-command-map
    "u" "Toggle lsp-ui-mode"
    "k" "Toggle lsp-ui-doc-mode"
    "U" "Global lsp-ui-mode"
    "K" "Global lsp-ui-doc-mode"
    "e" "LSP UI list errors"
    "i" "LSP UI Imenu"))

(provide 'lsp-conf)

;;; lsp-conf.el ends here.
