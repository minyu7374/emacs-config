;;; ui-conf.el -- UI Config
;;; Commentary:
;;      UI设置

;;; Code:


(+global-word-wrap-mode +1)

(setq-default fill-column 120
              delete-trailing-lines t)

(global-goto-address-mode t)

(after! lsp-ui
   (setq lsp-ui-doc-enable t)
   ;;top/bottom/at-point
   (setq lsp-ui-doc-position 'at-point)
   ;; (setq lsp-ui-doc-side 'right)
   (setq lsp-ui-doc-delay 0.5)
   (setq lsp-ui-doc-show-with-cursor t)
   ;; (setq lsp-ui-doc-show-with-mouse nil)

   ;; (setq lsp-ui-imenu-window-width 30)
   ;; (setq lsp-ui-imenu--custom-mode-line-format t)
   ;; (setq lsp-ui-imenu-auto-refresh t)
  )

(provide 'ui-conf)

;;; ui-conf.el ends here.
