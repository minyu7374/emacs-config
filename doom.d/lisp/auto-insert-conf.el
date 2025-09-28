;;; auto-insert-conf.el -- auto insert snippets for file
;;; Commentary:
;;      自动添加代码片段

;;; Code:

;; (yas-global-mode 1)
;; (setq yas-snippet-dirs '("~/.doom.d/snippets"))
;; (yas-reload-all)
(defun auto-insert-header()
  "Automatically insert a file header snippet for supported file types."
  (when (zerop (buffer-size))                   ; 确保文件为空
    (yas-expand-snippet (yas-lookup-snippet "header" major-mode))))

(auto-insert-mode 1)
;; (setq auto-insert-query nil)                    ; 自动插入时不询问
(setq auto-insert-alist
      (append '((("\\.c\\'" . "C header") . auto-insert-header)
                (("\\.cpp\\'" . "C++ header") . auto-insert-header)
                (("\\.go\\'" . "Go header") . auto-insert-header)
                (("\\.py\\'" . "Python header") . auto-insert-header)
                (("\\.sh\\'" . "Shell header") . auto-insert-header))
              auto-insert-alist))

(provide 'auto-insert-conf)

;;; auto-insert-conf.el ends here.
