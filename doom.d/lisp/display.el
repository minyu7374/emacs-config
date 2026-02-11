;;; display.el -- Doom Display config -*- lexical-binding: t; -*-
;;; Commentary:
;;      Proxy 控制

;;; code:
;;; Commentary:
;;      Doom图形界面相关配置

;;; Code:
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

;; (defvar default-font-size 14 "Default font size for doom theme.")
;; (defvar big-font-size 18 "Big font size for doom theme.")
;; ;; 实测现在用的字体，缩放1.25倍后对齐两个英文字母，但是中文字体显得特别大，不太美观，平时还是用1.1吧
;; (defvar chinese-font-rescale 1.1 "Chinese font scale ratio for doom theme.")

;; ;;(condition-case nil (require 'font-conf) (error nil))
;; (require 'font-conf nil :noerror)

(defun doom/reload-custom-font()
  "Set custom fonts for doom theme."
  (interactive)

  ;; JetBrainsMono/Consolas[ NF(M)]  ExtraLight/Thin
  (let ((default-font "JetBrainsMono NFM")
        ;; FZLiBian-S02/FZTieJinLiShu-S17/Microsoft YaHei
        (chinese-font "FZLiBian-S02"))

    (setq doom-font (font-spec :family default-font :weight 'light :slant 'italic :size default-font-size)
          ;; doom-variable-pitch-font (font-spec :family default-font :weight 'light :slant 'italic :size default-font-size)
          ;; doom-serif-font (font-spec :family default-font :weight 'light :slant 'italic :size default-font-size)
          doom-big-font (font-spec :family default-font :weight 'light :slant 'italic :size big-font-size))
    (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" default-font default-font-size) :weight 'light :slant 'italic)
    (set-face-attribute 'bold nil :font (format "%s:pixelsize=%d" default-font default-font-size) :weight 'bold :slant 'normal)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese-font)))
    (setq face-font-rescale-alist `((,chinese-font . ,chinese-font-rescale)))))

(add-hook 'doom-load-theme-hook (lambda() (if (display-graphic-p) (doom/reload-custom-font))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dracula)
;; doom-dracula/doom-one/doom-tomorrow-night
(setq doom-theme (if (display-graphic-p) 'doom-dracula 'doom-one))

;; fullscreen (不再区分 os-fullscreen-type)
(set-frame-parameter nil 'fullscreen 'maximized)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; when using emacs daemon
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-frame-parameter nil 'fullscreen 'maximized)
              (if (display-graphic-p frame)
                  (setq doom-theme 'doom-dracula)
                (progn
                  (setq doom-theme 'doom-one)
                  (menu-bar-mode -1))))))

;; (map! :leader
;;       (:prefix ("hrc" . "custom")
;;        :desc "set custom font for doom theme" :nv "f" #'doom/reload-custom-font))

(map! :map help-map
      "rc" nil
      "rcf" #'doom/reload-custom-font)

(after! which-key
  ;; (which-key-add-key-based-replacements "SPC h r c" "custom"))
  ;; 从doom源码help-map部分抄写，新增 rc 的描述
  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:<\\(?:\\(?:f1\\|help\\)>\\)\\|C-h\\|%s h\\) r c\\'" prefix-re))
                  nil . "custom")
                which-key-replacement-alist)))

;; icons
(after! dired
  (add-hook! 'dired-mode 'all-the-icons-dired-mode))

;; 光标离窗口顶部或底部多少行时，Emacs 将自动滚动窗口，以使光标保持在指定的行数之外，改善阅读体验
(setq scroll-margin 5)

;; 缩进提示
(after! indent-bars
  (setq indent-bars-treesit-support t)  ;; 使用 treesit AST 判断缩进
  (setq indent-bars-pattern ".") ;; 普通缩进用虚点
  (setq indent-bars-color '(highlight :face-bg t :blend 0.15)) ;; 普通缩进颜色（浅灰）
  (setq indent-bars-highlight-current-depth '(:face default :blend 0.3)) ;; 高亮当前缩进层级（更亮一些）
  ;; 其它推荐设置
  (setq indent-bars-width-frac 0.2)
  (setq indent-bars-pad-frac 0.1)
  (setq indent-bars-display-on-blank-lines t))

;; treemacs 不用显示行号
(after! treemacs
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local display-line-numbers nil))))

;; 控制 * 搜索不跳转，原本的行为绑定到 g*
(after! evil
  ;; 实际实现是照抄evil-ex-search-word-forward的逻辑，只是为了不跳转将count设置为0
  (defun my/highlight-word-no-jump (&optional symbol)
    "Highlight word/symbol at point without jumping, identical logic to evil-ex-search-word-forward."
    (interactive (list evil-symbol-word-search))
    (evil-ex-start-word-search nil 'forward 0 symbol))

  (map! :n "g*" #'evil-ex-search-word-forward
        :n "*"  #'my/highlight-word-no-jump))

(provide 'display)

;;; display.el ends here.
