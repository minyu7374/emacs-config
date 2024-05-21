;;; mpl-conf.el --- ampl conf
;;; Commentary:
;;      MPL数学规划语言配置

;;; code:

;; MathProg支持 排除go.mod: /^(?!.*go).*\.mod$/ /([^o]|[^g]o)+\.mod$/
;; (add-to-list 'auto-mode-alist '("\\([^o]\\|[^g]o\\)+\\.mod\\'" . gmpl-mode))

;; 改为Ampl mode
(setq auto-mode-alist
      (cons '("\\([^o]\\|[^g]o\\)+\\.mod\\'" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.run$" . ampl-mode) auto-mode-alist))
;; (setq interpreter-mode-alist
;;       (cons '("\\.run$" . ampl-mode)
;;             interpreter-mode-alist))

(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)

(provide 'mpl-conf)

;;; mpl-conf.el ends here
