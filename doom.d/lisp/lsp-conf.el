;;; lsp-conf.el -- LSP Config
;;; Commentary:
;;      LSP设置

;;; Code:

;; lsp-sommand-map: <leader>cl
(after! lsp-mode
  (map! :map lsp-command-map "s" #'+lsp/switch-client)
  (which-key-add-keymap-based-replacements lsp-command-map "s" "Switch LSP client"))

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
  (global-set-key (kbd "\C-cli") 'lsp-ui-imenu)

  (map! :map lsp-command-map
        "u" #'lsp-ui-mode
        "k" #'lsp-ui-doc-mode
        "U" #'global-lsp-ui-mode
        "K" #'global-lsp-ui-doc-mode
        "i" #'lsp-ui-imenu)
  (which-key-add-keymap-based-replacements lsp-command-map
    "u" "Toggle lsp-ui-mode"
    "k" "Toggle lsp-ui-doc-mode"
    "U" "Global lsp-ui-mode"
    "K" "Global lsp-ui-doc-mode"
    "i" "LSP UI Imenu"))

(provide 'lsp-conf)

;;; lsp-conf.el ends here.
