(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (exec-path-from-shell-copy-env "GOPATH")
  ;; (exec-path-from-shell-copy-env "RIME_PATH")
  )

(provide 'exec-shell-path)
