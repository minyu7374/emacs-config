;;; input.el -- input config
;;; Commentary:
;;;      输入行为相关配置

;;; Code:

(setq default-input-method "rime")

(global-set-key (kbd "C-\\") 'toggle-input-method)
(map! :leader
      (:prefix "t"
       :desc "toggle input method" :nv "i" #'toggle-input-method))

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (rime-user-data-dir (concat doom-local-dir "rime/"))
  ;; max下需要单独下载librime链接库文件
  ;; (rime-emacs-module-header-root (if IS-MAC "/Applications/Emacs.app/Contents/Resources/include" nil))
  ;;(rime-librime-root (if IS-MAC "~/.local/lib/librime/dist" nil))
  ;; MacPorts: port install emacs-app librime-devel
  (rime-emacs-module-header-root (if IS-MAC "/Applications/MacPorts/Emacs.app/Contents/Resources/include" nil))
  (rime-librime-root (if IS-MAC "/opt/local" nil))
  :config
  (setq rime-show-candidate (if (display-graphic-p) 'posframe 'popup))

  ;; 在 evil-normal-state 中、在英文字母后面以及代码中自动使用英文
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p))

  ;; support shift-l, shift-r, control-l, control-r，需在 deafult.custom.yaml 配置中保持一致，
  (setq rime-inline-ascii-trigger 'shift-l)

  ;; Any single character that not trigger auto commit
  (setq rime-inline-ascii-holder ?x)

  (global-set-key (kbd "C-,") 'rime-inline-ascii)
  (global-set-key (kbd "C-.") 'rime-force-enable)
  (global-set-key (kbd "C-|") 'rime-select-schema)

  (map! :leader
        (:prefix ("z" . "zh_cn")
         :desc "rime inline ascii" :nv "i" #'rime-inline-ascii
         :desc "force rime" :nv "f" #'rime-force-enable
         :desc "rime select scheme" :nv "s" #'rime-select-schema))
  )

;; 拼写检查
;;(setq ispell-program-name "aspell")
;;(setq ispell-dictionary "en_US")

(provide 'input)

;;; input.el ends here.
