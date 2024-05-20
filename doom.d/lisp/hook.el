;; icons
(after! dired
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook! 'dired-mode 'all-the-icons-dired-mode)
  )

;; table align
;; (after! org-mode
;;   (add-hook! 'org-mode 'valign-mode)
;;   )
;; (after! markdown-mode
;;   (add-hook! 'markdown-mode 'valign-mode)
;;   )

(provide 'hook)
