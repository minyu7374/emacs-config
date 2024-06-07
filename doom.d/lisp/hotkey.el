;;; hotkey.el -- Set Custom HotKey
;;; Commentary:
;;;     自定义的一些快捷键
;;; code:

;; DIY 命令前缀
(map! :leader
      (:prefix ("y" . "DIY")))

;; ;; 禁用鼠标
;; (xterm-mouse-mode -1)
;; (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
;;              [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
;;              [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
;;              [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
;;              [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
;;   (global-unset-key k))

;; comment
(global-set-key (kbd "\C-c\C-c") 'comment-line)
(global-set-key (kbd "\C-cc") 'comment-line)
(map! :leader (:prefix "c"
               :desc "comment or uncomment current line" :nv "m" #'comment-line))

;; set mark ctrl+space 和常用输入法切换快捷键冲突
(global-unset-key (kbd "C-SPC"))
;; alt+space 在Linux kde上是kruner，command+space是Albert(Linux)/Spotlight(Mac)
(global-set-key (kbd "C-S-SPC") 'set-mark-command)

;; insert current datetime
(defun insert-current-datetime ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))
;; (insert (format-time-string "%Y-%m-%d %r")))

(global-set-key (kbd "\C-c\C-t") 'insert-current-datetime)
(global-set-key (kbd "\C-cit") 'insert-current-datetime)
(map! :leader (:prefix "i"
               :desc "insert current datetime" :nv "t" #'insert-current-datetime))

;; imenu (lsp-sommand-map: <leader>cl)
(global-set-key (kbd "\C-c\C-u") 'lsp-ui-mode)
(map! :map lsp-command-map
      "u" #'lsp-ui-mode
      "c" #'+lsp/switch-client)

(after! lsp-ui
  (global-set-key (kbd "\C-c\C-m") 'lsp-ui-imenu)
  (map! :map lsp-command-map
        "m" #'lsp-ui-imenu)
  )

;; window move
;; 和emacs本身的快捷键产生冲突，不使用了
;; (if (featurep 'tmux-pane)
;;     (use-package! tmux-pane
;;       :config
;;       (tmux-pane-mode)
;;       (map! :leader
;;             (:prefix ("v" . "tmux pane")
;;              :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
;;              :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
;;              :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
;;              :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
;;              :desc "Close pane" :nv "c" #'tmux-pane-close
;;              :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
;;       (map! :leader
;;             (:prefix "t"
;;              :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
;;              :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal))
;;       )
;;       (progn
;;         (global-set-key (kbd "C-\\") 'evil-window-prev)
;;         (global-set-key (kbd "C-k") 'evil-window-up)
;;         (global-set-key (kbd "C-j") 'evil-window-down)
;;         (global-set-key (kbd "C-h") 'evil-window-left)
;;         (global-set-key (kbd "C-l") 'evil-window-right))
;;       )

(provide 'hotkey)

;;; hotkey.el ends here.
