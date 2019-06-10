;;;; look & feel
;; (global-hl-line-mode 0)
(spacemacs/enable-transparency)

;;;; chinese
;; youdao
(global-set-key (kbd "\C-xy") 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)

;; input
;; Make sure the following comes before `(fcitx-aggressive-setup)'
(setq fcitx-active-evil-states '(insert emacs hybrid)) ; if you use hybrid mode
(fcitx-aggressive-setup)
(fcitx-prefix-keys-add "M-m") ; M-m is common in Spacemacs
(fcitx-prefix-keys-turn-on)
(fcitx-isearch-turn-on)
(setq fcitx-use-dbus t) ; uncomment if you're using Linux
;; (global-set-key (kbd "C-\\") 'toggle-input-method)
;; (setq default-input-method "chinese-py")
;; (setq default-input-method "pyim")
;; (setq pyim-default-scheme 'rime)
;; (setq pyim-page-style 'one-line)
;; (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。
;; (global-set-key (kbd "M-i") 'pyim-convert-string-at-point) ;将光标处的拼音或者五笔字符串转换为中文

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
  (treemacs-resize-icons 14)
  )

(provide 'personal-preference)
