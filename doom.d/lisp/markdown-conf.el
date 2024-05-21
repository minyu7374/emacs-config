;;; markdown-conf.el -- Markdown Language Support
;;; Commentary:
;;;      markdown语言支持相关配置，主要是preview.

;;; Code:

(after! markdown-mode
  (setq markdown-split-window-direction 'right)

  (defun marp-preview()
    "Markdown Preview by Marp"
    (interactive)
    ;; (async-shell-command (format "marp -p '%s'" buffer-file-name))
    (start-process-shell-command "marp-preview" nil (format "marp -p '%s'" buffer-file-name))
    )

  (defun marp-export-open()
    (interactive)
    (let ((os-open (cond (IS-MAC "open") (IS-LINUX "xdg-open")))
          (out-ppt (concat (shell-command-to-string "mktemp") ".pptx")))
      (start-process-shell-command
       "marp_export_open" nil
       (format "marp --pptx --allow-local-files '%s' -o '%s' && %s '%s'" buffer-file-name out-ppt os-open out-ppt)))
    )

  (defun reveal-preview()
    "Markdown Preview by Reveal"
    (interactive)
    (let ((reveal-root (concat doom-local-dir "reveal.js"))
          (custom-css (concat doom-local-dir "etc/reveal.js/custom.css"))
          (os-open (cond (IS-MAC "open") (IS-LINUX "xdg-open")))
          ;; 如果markdown文件里有相对路径资源的引用，随机html文件将不合适，因此改为与原文件同路径同名的html文件
          ;; (out-html (concat (shell-command-to-string "mktemp") ".html")))
          (out-html (concat (file-name-sans-extension buffer-file-name) ".html")))
      (start-process-shell-command
       "md2reveal_preview" nil
       (format "pandoc -t revealjs -s --mathjax --toc -V theme=sky -V revealjs-url='file://%s' --include-in-header='%s' -o '%s' '%s' && %s '%s'"
               reveal-root custom-css out-html buffer-file-name os-open out-html)))
    )

  (map! :map markdown-mode-map
        :localleader
        "P" #'marp-preview
        "R" #'reveal-preview
        "E" #'marp-export-open))

(provide 'markdown-conf)

;;; markdown-conf.el ends here;
