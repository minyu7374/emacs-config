;;; cc-conf.el -- C/C++ Config
;;; Commentary:
;;      C/C++ 语言服务协议配置使用lsp/ccls

;;; Code:

;; lsp + clangd
(after! lsp-clangd
  (set-lsp-priority! 'clangd 2)
  ;;emacs里不过多设置参数，行为控制还是通过 .clangd 配置更好
  (setq lsp-clients-clangd-args
        `(,(format "-j=%d" (max 1 (/ (* (doom-system-cpus) 2) 3)))
          ;; 使用内存存储 PCH，可能加快索引速度
          "--pch-storage=memory"
          ;; Linux 才支持 malloc-trim
          ,@(when (eq system-type 'gnu/linux) '("--malloc-trim"))
          ;; "--background-index"
          ;; "--clang-tidy"
          ;; "--all-scopes-completion"
          ;; "--completion-style=detailed"
          ;; "--header-insertion=iwyu"
          ;; "--header-insertion-decorators=0"
          ;; "--limit-results=25"
          ;; "--limit-references=100"
          )))

;; lsp + ccls
(after! ccls
  ;; (set-lsp-priority! 'ccls 2)
  ;; https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
  (setq ccls-initialization-options
        (append ccls-initialization-options
                `(:index (:comments 2
                          :threads ,(max 1 (/ (* (doom-system-cpus) 2) 3))
                          :initialBlacklist '(".ccls-cache" ".cache" "build" "external"))
                  ;; :diagnostics (:onChange 500) ;; 降低诊断频率
                  :completion (:detailedLabel t)))))

;; cmakelsp => neocmakelsp stdio 代替 cmake-language-server
(setq lsp-cmake-server-command "cmakelsp")

;;; eglot
;; (set-eglot-client! 'cc-mode '("clangd" "-j=12" "--clang-tidy"))
;; (set-eglot-client! 'cc-mode '("ccls" "--init={\"index\": {\"threads\": 0}}"))

(provide 'cc-conf)

;;; cc-conf.el ends here.
