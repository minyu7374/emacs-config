;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "minyu"
      user-mail-address "minyu7374@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; 中英文对齐 参考 https://emacs-china.org/t/org-mode/440/9
(defun set-font()
  (interactive)
  (let ((default-font "Consolas")
        (chinese-font "FZTieJinLiShu-S17S")
        (default-font-size 14)
        (big-font-size 20)
        (chinese-font-rescale 1.2))

    (setq doom-big-font (font-spec :family default-font :slant 'italic :size big-font-size)
          doom-variable-pitch-font (font-spec :family default-font :slant 'italic :size default-font-size)
          doom-serif-font (font-spec :family default-font :slant 'italic :weight 'light))
    (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" default-font default-font-size) :slant 'italic)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese-font)))
    (setq face-font-rescale-alist `((,chinese-font . ,chinese-font-rescale)))
    )
  )

(if (display-graphic-p)
    (set-font))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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

(set-frame-parameter nil 'fullscreen 'fullboth)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(+global-word-wrap-mode +1)

(setq-default fill-column 120
              delete-trailing-lines t)

;;;; when using emacs daemon
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            (select-frame new-frame)
            (set-frame-parameter nil 'fullscreen 'fullboth)
            (if (display-graphic-p)
                (set-font)
              (menu-bar-mode -1))
            (doom/reload-font)))

;;;; comment
(global-set-key (kbd "\C-cc") 'comment-line)
(map! :leader
      (:prefix "c"
       :desc "comment or uncomment current line" :nv "m" #'comment-line))

;;;; set mark ctrl+space 和常用输入法切换快捷键冲突
(global-unset-key (kbd "C-SPC"))
;; alt+space 在Linux kde上是kruner，command+space是Albert(Linux)/Spotlight(Mac)
(global-set-key (kbd "C-S-SPC") 'set-mark-command)

;;;; insert current datetime
(defun insert-current-datetime ()
  "Insert date at point."
  (interactive)
  ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert (format-time-string "%Y-%m-%d %r")))

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

;;;; 输入法
(setq default-input-method "rime")

(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "S-SPC") 'toggle-input-method)
(map! :leader
      (:prefix "t"
       :desc "toggle input method" :nv "i" #'toggle-input-method))

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  ;; max下需要单独下载librime链接库文件
  (if IS-MAC (rime-librime-root "~/.local/lib/librime/dist"))
  (rime-user-data-dir (concat doom-local-dir "rime/"))
  :config
  (if (display-graphic-p)
      (setq rime-show-candidate 'posframe)
    (setq rime-show-candidate 'popup))

  ;; 在 evil-normal-state 中、在英文字母后面以及代码中自动使用英文
  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p))

  ;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)

  ;; 在有编码的状态下使用 rime-inline-ascii 命令可以切换状态
  (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)

  ;; Any single character that not trigger auto commit
  (setq rime-inline-ascii-holder ?x)

  (define-key rime-mode-map (kbd "C-\"") 'rime-force-enable) ;; 强制中文
  (define-key rime-mode-map (kbd "C-'") 'rime-select-schema)

  (global-set-key (kbd "\C-czf") 'rime-force-enable)
  (global-set-key (kbd "\C-czs") 'rime-select-schema)
  (map! :leader
        (:prefix ("z" . "chinaese")
         :desc "force rime" :nv "f" #'rime-force-enable
         :desc "rime select scheme" :nv "s" #'rime-select-schema))
  )

;;;; 和tmux无缝跳转
(use-package! tmux-pane
  :config
  (tmux-pane-mode)
  (map! :leader
        (:prefix ("v" . "tmux pane")
         :desc "Open vpane" :nv "o" #'tmux-pane-open-vertical
         :desc "Open hpane" :nv "h" #'tmux-pane-open-horizontal
         :desc "Open hpane" :nv "s" #'tmux-pane-open-horizontal
         :desc "Open vpane" :nv "v" #'tmux-pane-open-vertical
         :desc "Close pane" :nv "c" #'tmux-pane-close
         :desc "Rerun last command" :nv "r" #'tmux-pane-rerun))
  (map! :leader
        (:prefix "t"
         :desc "vpane" :nv "v" #'tmux-pane-toggle-vertical
         :desc "hpane" :nv "h" #'tmux-pane-toggle-horizontal))
  ;; tmux-pane 会把 C-\ 设置为 omni-window-last，这里恢复为输入法开关
  (map! :map tmux-pane--override-keymap
        "C-\\" #'toggle-input-method)
  )

;;;; mac 下环境变量
;; Mac GUI 只有基础的环境变量集，需加载shell环境变量
;;(when (memq window-system '(mac ns x))
(if (and IS-MAC (display-graphic-p))
    (use-package! exec-path-from-shell
      :config
      ;;(setq exec-path-from-shell-arguments nil)
      (setq exec-path-from-shell-arguments '("-l"))
      (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "PYTHONPATH"))
      (exec-path-from-shell-initialize)
      )
  )

;;max下shell脚本自动补全比较慢
(after! sh-script
  (if IS-MAC
      (set-company-backend! 'sh-mode nil))
  )

;; icons
(after! dired
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook! 'dired-mode 'all-the-icons-dired-mode)
  )

(after! markdown-mode
  (setq markdown-split-window-direction 'right)

  (defun marp-preview()
    (interactive)
    ;; (async-shell-command (format "marp -p '%s'" buffer-file-name))
    (start-process-shell-command "marp-preview" nil (format "marp -p '%s'" buffer-file-name))
    )

  (defun marp-export-open()
    (interactive)
    (let ((os-open (cond (IS-MAC "open") (IS-LINUX "xdg-open")))
          ;; 如果markdown文件里有相对路径资源的引用，随机html文件将不合适，因此改为与原文件同路径同名的html文件
          (out-ppt (concat (shell-command-to-string "mktemp") ".pptx")))
      (start-process-shell-command
       "marp_export_open" nil
       "marp" "--pptx --allow-local-files"
       (format "'%s' -o '%s' && %s '%s'" buffer-file-name out-ppt os-open out-ppt))
      )
    )

  (defun reveal-preview()
    (interactive)
    (let ((reveal-root (concat doom-local-dir "reveal.js"))
          (custom-css (concat doom-local-dir "etc/reveal.js/custom.css"))
          (os-open (cond (IS-MAC "open") (IS-LINUX "xdg-open")))
          ;; 如果markdown文件里有相对路径资源的引用，随机html文件将不合适，因此改为与原文件同路径同名的html文件
          ;; (out-html (concat (shell-command-to-string "mktemp") ".html")))
          (out-html (concat (file-name-sans-extension buffer-file-name) ".html")))
      (start-process-shell-command
       "md2reveal_preview" nil
       "pandoc" "-t revealjs -s --mathjax --toc -V theme=sky"
       (format "-V revealjs-url='file://%s' --include-in-header='%s' -o '%s' '%s' && %s '%s'"
               reveal-root custom-css out-html buffer-file-name os-open out-html))
      )
    )

  (map! :map markdown-mode-map
        :localleader
        "P" #'marp-preview
        "R" #'reveal-preview
        "E" #'marp-export-open)
  )

(after! lsp-ui
  (map! :map lsp-command-map
        "m" #'lsp-ui-imenu)
  )


(add-to-list 'load-path "~/.doom.d/config-lisp")
(require 'auto-insert-header)
