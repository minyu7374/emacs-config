;;; cc-conf.el -- C/C++ Config
;;; Commentary:
;;      C/C++ 语言服务协议配置使用lsp/ccls

;;; Code:

;; lsp-mode
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=12"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"
          "--limit-results=25"
          ;; "--limit-references=100"     ;; 限制引用数量
          "--pch-storage=memory"))        ;; 使用内存存储 PCH，可能加快索引速度
  (set-lsp-priority! 'clangd 2))

(after! ccls
  ;; https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
  (setq ccls-initialization-options
        (append ccls-initialization-options
                `(:index (:comments 2
                          :threads ,(max 1 (/ (* (doom-system-cpus) 2) 3))
                          :initialBlacklist '(".ccls-cache" ".cache" "build" "external" "third_party"))
                  ;; :diagnostics (:onChange 500) ;; 降低诊断频率
                  :completion (:detailedLabel t))))
  (set-lsp-priority! 'ccls 1)) ; optional as ccls is the default in Doom

;;; eglot
;; (set-eglot-client! 'cc-mode '("clangd" "-j=12" "--clang-tidy"))
;; (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 0}}"))

(provide 'cc-conf)

;;; cc-conf.el ends here.
