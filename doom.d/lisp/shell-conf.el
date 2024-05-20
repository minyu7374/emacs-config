;; Mac(GUI) 只有基础的环境变量集，需加载shell环境变量
;; (when (memq window-system '(mac ns x))
(use-package! exec-path-from-shell
  :if IS-MAC
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "PYTHONPATH" "C_INCLUDE_PATH" "LSP_USE_PLISTS"))
  :config
  ;;(setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)
  )

;; max下shell脚本自动补全比较慢
(after! sh-script
  (if IS-MAC
      (set-company-backend! 'sh-mode nil))
  )

(provide 'shell-conf)
