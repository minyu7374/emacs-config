;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "minyu"
      user-mail-address "minyu7374@gmail.com")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; lisp
(add-to-list 'load-path "~/.doom.d/lisp")
(require 'display)
(require 'input-method)
(require 'markdown)
(require 'tmux)
;; (require 'eaf-conf)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; 禁用鼠标
(xterm-mouse-mode -1)
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

(+global-word-wrap-mode +1)

(setq-default fill-column 120
              delete-trailing-lines t)

;; 拼写检查
;;(setq ispell-program-name "aspell")
;;(setq ispell-dictionary "en_US")

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

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Mac(GUI) 只有基础的环境变量集，需加载shell环境变量
;; (when (memq window-system '(mac ns x))
(use-package! exec-path-from-shell
  :if IS-MAC
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "PYTHONPATH" "C_INCLUDE_PATH"))
  :config
  ;;(setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)
  )

;; max下shell脚本自动补全比较慢
(after! sh-script
  (if IS-MAC
      (set-company-backend! 'sh-mode nil))
  )

;; icons
(after! dired
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook! 'dired-mode 'all-the-icons-dired-mode)
  )

;; imenu
(after! lsp-ui
  (map! :map lsp-command-map
        "m" #'lsp-ui-imenu)
  )

;; MathProg支持 排除go.mod: /^(?!.*go).*\.mod$/ /([^o]|[^g]o)+\.mod$/
(add-to-list 'auto-mode-alist '("\\([^o]\\|[^g]o\\)+\\.mod\\'" . gmpl-mode))
