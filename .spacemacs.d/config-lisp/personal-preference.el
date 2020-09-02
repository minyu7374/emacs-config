;;;; look & feel
;; (global-hl-line-mode 0)
(spacemacs/enable-transparency)

;;; org-mode load-languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (dot . t)))

;;;; zeal(offline API documentation browser inspired by Dash)
(global-set-key "\C-cd" 'zeal-at-point)

;;;; chinese
;; youdao
(global-set-key (kbd "\C-xy") 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)

;; input
(global-set-key (kbd "C-\\") 'toggle-input-method)

;; fcitx
;; ;; Make sure the following comes before `(fcitx-aggressive-setup)'
;; (setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
;; (fcitx-aggressive-setup)
;; (fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
;; (fcitx-prefix-keys-turn-on)
;; (fcitx-isearch-turn-on)
;; ;; (setq fcitx-use-dbus t) ; for Linux

;; (setq default-input-method "chinese-py")

;; pyim
(setq default-input-method "pyim")

;; 模糊音
(setf pyim-fuzzy-pinyin-alist '(("z" "zh") ("c" "ch") ("s" "sh")))

;; 默认双行，使用popup可能会有显示错位，而单行数字和文字连一起展示不方便确认选项
;; (setq pyim-page-style 'one-line)
(setq pyim-page-length 7)

;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 C-z 快捷键，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; 将光标处的拼音或者五笔字符串转换为中文
(global-set-key (kbd "\C-z") 'pyim-convert-string-at-point)

;; 开启拼音搜索功能
(pyim-isearch-mode 1)

;; 标点符号
;; (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
;; (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
(setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。

(global-set-key (kbd "C-;") 'pyim-delete-word-from-personal-buffer)

;;;; tab
;; (global-set-key (kbd "TAB") 'self-insert-command)
;; (setq-default default-tab-width 4)
;; (setq-default indent-tabs-mode nil)

;;;; insert current datetime
(defun insert-current-datetime ()
  "Insert date at point."
    (interactive)
    ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (format-time-string "%Y-%m-%d %r")))
(global-set-key (kbd "\C-xt") 'insert-current-datetime)
(spacemacs/set-leader-keys "ot" 'insert-current-datetime)

;;;; program language
;; Bind clang-format-region to C-M-tab in all modes:
(global-set-key [C-M-tab] 'clang-format-region)

;; js indent
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

;; treemacs icons
(with-eval-after-load "treemacs"
  (treemacs-resize-icons 12)
  )

(provide 'personal-preference)
