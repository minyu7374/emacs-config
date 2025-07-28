;;; syntax-check.el -- Syntax Check Config
;;; Commentary:
;;      语法检查设置

;;; Code:

;; Python 只开启一个 Flycheck 检查工具
(defvar +python-flycheck-checkers
  '(python-ruff python-pylint python-pyright python-flake8 python-pyflakes python-pycompile python-mypy lsp)
  "List of available Flycheck checkers for Python.")
(defvar +python-flycheck-active-checker 'python-pyright
  "Global active Flycheck checker for Python buffers.")

(defun set-python-checker ()
  "Set Flycheck checker for the current Python buffer."
  (let ((disabled-checkers (remove +python-flycheck-active-checker +python-flycheck-checkers)))
    (message "Using Flycheck checker for Python: %s" +python-flycheck-active-checker)
    (setq-local flycheck-checker +python-flycheck-active-checker)
    (setq-local flycheck-disabled-checkers disabled-checkers)))

(defun switch-python-checker ()
  "Interactively switch Python Flycheck checker."
  (interactive)
  (setq +python-flycheck-active-checker
        (intern (completing-read "Set active Python checker: " (mapcar #'symbol-name +python-flycheck-checkers)
                                 nil t (symbol-name +python-flycheck-active-checker))))
  (set-python-checker))

(after! python
  (add-hook 'python-mode-hook #'set-python-checker)
  ;; (add-hook 'python-mode-hook (lambda () (setq-local lsp-diagnostics-provider :none)))
  (map! :mode python-mode
        :leader
        (:prefix "e"
         :desc "Switch Flycheck checker" "s" #'switch-python-checker)))

(map! :leader
      (:prefix ("e" . "errors")
       :desc "Next error"       "n" #'flycheck-next-error
       :desc "Previous error"   "p" #'flycheck-previous-error
       :desc "List errors"      "l" #'flycheck-list-errors
       :desc "Disable checker"  "d" #'flycheck-disable-checker
       :desc "Checker setup"   "v" #'flycheck-verify-setup
       ))

(map! (:prefix "g"
       :nv "[" #'flycheck-previous-error
       :nv "]" #'flycheck-next-error))

(provide 'syntax-check)

;;; syntax-check.el ends here.
