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
;; 中英文对齐
;;https://emacs-china.org/t/org-mode/440/9
;; (setq fonts
;;       (cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
;;             ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Zen Hei"))
;;             ((eq system-type 'windows-nt) '("Consolas"  "Microsoft Yahei"))))
;; (set-face-attribute 'default nil :font
;;                     (format "%s:pixelsize=%d" (car fonts) 14))
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family (car (cdr fonts)))))
;; ;; Fix chinese font width and rescale
;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))

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

;;;; insert current datetime
(defun insert-current-datetime ()
  "Insert date at point."
  (interactive)
  ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert (format-time-string "%Y-%m-%d %r")))
(global-set-key (kbd "\C-cot") 'insert-current-datetime)

(+global-word-wrap-mode +1)

(setq-default fill-column 120
              delete-trailing-lines t)

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

;; 输入法
(setq default-input-method "rime")
(global-set-key (kbd "C-\\") 'toggle-input-method)

(use-package! pyim
  :after-call after-find-file pre-command-hook
  :init
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
  )

(after! pyim
  (setq pyim-default-scheme "quanpin")

  ;; pyim-greatdict 词库
  ;; (pyim-greatdict-enable)

  ;; 模糊音
  (setf pyim-fuzzy-pinyin-alist '(("z" "zh") ("c" "ch") ("s" "sh")))

  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))
  (setq pyim-page-length 7)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-
  ;; 中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用快捷键，强制将光标前的拼音字符串转换为中文。
  (if (display-graphic-p)
      (setq-default pyim-english-input-switch-functions
                    '(pyim-probe-dynamic-english
                      pyim-probe-isearch-mode
                      pyim-probe-program-mode
                      pyim-probe-org-structure-template)))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 将光标处的拼音或者五笔字符串转换为中文
  (global-set-key (kbd "C-\|") 'pyim-convert-string-at-point)

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 标点符号
  ;; (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
  ;; (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
  (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。

  (global-set-key (kbd "C-;") 'pyim-delete-word-from-personal-buffer)
  )

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-user-data-dir (concat doom-cache-dir "rime/"))
  )

(after! rime
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

  ;; Any single character that not trigger auto commit
  (setq rime-inline-ascii-holder ?x)

  (define-key rime-mode-map (kbd "C-\"") 'rime-force-enable) ;; 强制中文
  (define-key rime-mode-map (kbd "C-'") 'rime-select-schema)
  )

;; icons
(after! dired
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook! 'dired-mode 'all-the-icons-dired-mode)
  )

(use-package! imenu-list
  :custom
  (imenu-list-position 'right)
  (imenu-list-size 0.2)
  )

(use-package! awesome-pair)

(after! awesome-pair
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'haskell-mode-hook
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'php-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'go-mode-hook
                 'css-mode-hook
                 'ruby-mode-hook
                 'coffee-mode-hook
                 'rust-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'json-mode-hook
                 'markdown-mode-hook
                 'minibuffer-inactive-mode-hook
                 ))
    (add-hook hook '(lambda () (awesome-pair-mode 1))))

  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
  )

(after! markdown-mode
  (setq markdown-split-window-direction 'right)

  (defun marp-preview()
    (interactive)
    ;; (async-shell-command (format "marp -p '%s'" buffer-file-name))
    (start-process-shell-command "marp-preview" nil (format "marp -p '%s'" buffer-file-name))
    )

  (defun reveal-preview()
    (interactive)
    (let ((reveal-root (concat doom-local-dir "reveal.js"))
          (custom-css (concat doom-cache-dir "reveal.js/custom.css"))
          (os-open (cond (IS-MAC "open") (IS-LINUX "xdg-open")))
          ;; 如果markdown文件里有相对路径资源的引用，随机html文件将不合适，因此改为与原文件同路径同名的html文件
          ;; (out-html (concat (shell-command-to-string "mktemp") ".html")))
          (out-html (concat (file-name-sans-extension buffer-file-name) ".html")))
      (start-process-shell-command
       "md2reveal" nil
       "pandoc" "-t revealjs -s --mathjax --toc -V theme=sky"
       (format "-V revealjs-url='file://%s' --include-in-header='%s' -o '%s' '%s' && %s '%s'"
               reveal-root custom-css out-html buffer-file-name os-open out-html))
      )
    )

  ;; (global-set-key (kbd "\C-cop") 'marp-preview)
  ;; (global-set-key (kbd "\C-cor") 'reveal-preview)
  (map! :map markdown-mode-map
        :localleader
        "P" #'marp-preview
        "R" #'reveal-preview)
  )
