;;;; disable mouse
(xterm-mouse-mode -1)
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
              [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
              [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
              [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
              [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
(global-unset-key k))

;;;; look & feel
;; (global-hl-line-mode 0)
(spacemacs/enable-transparency)

;;;; chinese
;; no need to type Chinese in minibuffer
;; (fcitx-aggressive-setup)

;; youdao
(global-set-key (kbd "\C-xy") 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)

;; input
;; (setq default-input-method "pyim")
;; (setq pyim-default-scheme 'rime)
;; (setq pyim-page-style 'one-line)
(setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。
(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "M-i") 'pyim-convert-string-at-point) ;将光标处的拼音或者五笔字符串转换为中文

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

(provide 'personal-preference)
