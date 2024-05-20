;; DIY 命令前缀
(map! :leader
      (:prefix ("y" . "DIY")))

;; 禁用鼠标
(xterm-mouse-mode -1)
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; comment
(global-set-key (kbd "\C-cc") 'comment-line)
(map! :leader
      (:prefix "c"
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

(global-set-key (kbd "\C-cit") 'insert-current-datetime)
(map! :leader
      (:prefix "i"
       :desc "insert current datetime" :nv "t" #'insert-current-datetime))

;; imenu
(after! lsp-ui
  (map! :map lsp-command-map
        "m" #'lsp-ui-imenu)
  )

(provide 'hotkey)
