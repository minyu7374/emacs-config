(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GOPATH")
  )

(provide 'exec-shell-path)
