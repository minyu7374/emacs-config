;;; input.el -- input config
;;; Commentary:
;;;      输入行为相关配置

;;; Code:

(setq default-input-method "rime")

(global-set-key (kbd "C-\\") 'toggle-input-method)
(map! :leader
      (:prefix "t"
       :desc "toggle input method" :nv "z" #'toggle-input-method))

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (rime-user-data-dir (concat doom-local-dir "rime/"))
  :init
  ;; MacOS下自动检测 emacs include 和 librime 路径
  ;; MacPorts: port install emacs-app librime-devel
  (when (eq system-type 'darwin)
    (setq rime-emacs-module-header-root
          (let ((dirs '("/Applications/MacPorts/Emacs.app/Contents/Resources/include/"
                        "/opt/homebrew/opt/emacs-mac/include/"
                        "/Applications/Emacs.app/Contents/Resources/include/")))
            (cl-find-if (lambda (d) (and (file-directory-p d) d)) dirs)))
    (setq rime-librime-root
          ;; MacPorts安装的librime(/opt/local)可以正常使用，homebrew的librime包用起来有问题，还是优先用github releases的（放在.local/lib/librime/dist）
          (let ((dirs '("/opt/local/"
                        "~/.local/lib/librime/dist/"
                        "/opt/homebrew/"
                        "/usr/local/")))
            (cl-find-if (lambda (d)
                          (let ((lib-dir (expand-file-name "lib" d)))
                            (and (file-directory-p lib-dir)
                                 (directory-files lib-dir nil "librime.*\\.dylib" t)
                                 d)))
                        dirs)))
    )
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
