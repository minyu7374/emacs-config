;;; syntax-check.el -- Syntax Check Config
;;; Commentary:
;;      语法检查设置

;;; Code:

;;;; Python Flycheck 切换，并持久生效
;;(defvar +python-flycheck-active-checker 'python-pyright
;;  "Global active Flycheck checker for Python buffers.")
;;
;;(after! flycheck
;;  (defun +python/select-flycheck-checker ()
;;    "Interactively switch Python Flycheck checker."
;;    (interactive)
;;    (setq +python-flycheck-active-checker
;;          (intern (completing-read "Set active Python checker: " (mapcar #'symbol-name flycheck-checkers)
;;                                   nil t (symbol-name +python-flycheck-active-checker))))
;;    (setq-local flycheck-checker +python-flycheck-active-checker))
;;
;;  (add-hook 'python-mode-hook (lambda() (setq-local flycheck-checker +python-flycheck-active-checker)))
;;  ;; (add-hook 'python-mode-hook (lambda () (setq-local lsp-diagnostics-provider :none)))
;;
;;  (map! :leader
;;        (:prefix ("e" . "errors")
;;         :desc "List errors"      "l" #'flycheck-list-errors
;;         :desc "Previous error"   "p" #'flycheck-previous-error
;;         :desc "Next error"       "n" #'flycheck-next-error
;;         :desc "Select checker"   "s" #'select-flycheck-checker
;;         :desc "Disable checker"  "d" #'flycheck-disable-checker
;;         :desc "Checker verify"   "v" #'flycheck-verify-setup))
;;
;;  (map! :mode python-mode
;;        :leader
;;        (:prefix "e"
;;         :desc "Select checker for Python" "s" #'+python/select-flycheck-checker))
;;
;;  ;; 已经创建了 [e 和 ]e 快捷键
;;  ;; (map! (:prefix "g"
;;  ;;        :nv "[" #'flycheck-previous-error
;;  ;;        :nv "]" #'flycheck-next-error))
;;  )

;; 支持多种语言的 flycheck checker 持久化切换
(defvar +flycheck-active-checkers (make-hash-table :test #'eq)
  "Hash table storing active Flycheck checkers per major mode.")

;; 实际发现，还是默认优先使用lsp的支持比较好，可以标记出具体错误位置，比如对 unsed 置灰
;; (puthash 'python-mode 'python-pyright +flycheck-active-checkers)
;; (defvar +flycheck-default-checkers
;;   '((python-mode . python-pyright)
;;     ;; 其他语言默认配置checker
;;     )
;;   "Default persistent Flycheck checkers per major mode.")
;; (dolist (pair +flycheck-default-checkers)
;;   (puthash (car pair) (cdr pair) +flycheck-active-checkers))

(after! flycheck
  (defun +flycheck/select-checker ()
    "Switch persistent Flycheck checker for current major mode."
    (interactive)
    (let* ((current (gethash major-mode +flycheck-active-checkers))
           (chosen (intern (completing-read (format "Select checker for %s: " major-mode)
                                            (mapcar #'symbol-name flycheck-checkers)
                                            nil t (when current (symbol-name current))))))
      (puthash major-mode chosen +flycheck-active-checkers)
      (setq-local flycheck-checker chosen)))

  (defun +flycheck/set-checker ()
    "Set buffer-local flycheck-checker from global hash."
    (let ((checker (gethash major-mode +flycheck-active-checkers)))
      (when checker
        (setq-local flycheck-checker checker))))

  ;; (dolist (hook '(python-mode-hook go-mode-hook sh-mode-hook c++-mode-hook c-mode-hook))
  ;;   (add-hook hook #'+flycheck/set-checker))
  (add-hook 'flycheck-mode-hook #'+flycheck/set-checker) ;; 针对所有语言

  (map! :leader
        (:prefix ("e" . "errors")
         :desc "List errors"      "l" #'flycheck-list-errors
         :desc "Previous error"   "p" #'flycheck-previous-error
         :desc "Next error"       "n" #'flycheck-next-error
         :desc "Select checker"   "s" #'flycheck-select-checker
         :desc "Select checker (persist)"   "S" #'+flycheck/select-checker
         :desc "Disable checker"  "d" #'flycheck-disable-checker
         :desc "Checker verify"   "v" #'flycheck-verify-setup)))

(provide 'syntax-check)

;;; syntax-check.el ends here.
