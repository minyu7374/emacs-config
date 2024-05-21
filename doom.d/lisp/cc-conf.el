;;; cc-conf.el -- C/C++ Config
;;; Commentary:
;;      C/C++ 语言服务协议配置使用lsp/ccls

;;; Code:

;; lsp-mode
;; (after! lsp-clangd
;;   (setq lsp-clients-clangd-args
;;         '("-j=8"
;;           "--background-index"
;;           "--clang-tidy"
;;           "--completion-style=detailed"
;;           "--header-insertion=never"
;;           "--header-insertion-decorators=0"))
;;   (set-lsp-priority! 'clangd 2))

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

;;; eglot
;; (set-eglot-client! 'cc-mode '("clangd" "-j=8" "--clang-tidy"))
;; (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 8}}"))

(provide 'cc-conf)

;;; cc-conf.el ends here.
