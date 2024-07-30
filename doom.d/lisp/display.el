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

(defvar default-font-size 13 "Default font size for doom theme.")
(defvar big-font-size 18 "Big font size for doom theme.")
(defvar chinese-font-rescale 1.1 "Chinese font scale ratio for doom theme.")

;; (require 'font-conf)
(condition-case nil (require 'font-conf) (error nil))

(defun doom/reload-custom-font()
  "Set custom fonts for doom theme."
  (interactive)

  ;; JetBrainsMono/Consolas[ NF]
  (let ((default-font "JetBrainsMono NF ExtraLight")
        ;; FZLiBian-S02/FZTieJinLiShu-S17/Microsoft YaHei
        (chinese-font "FZLiBian-S02"))

    (setq doom-font (font-spec :family default-font :slant 'italic :size default-font-size)
          doom-big-font (font-spec :family default-font :slant 'italic :size big-font-size)
          doom-variable-pitch-font (font-spec :family default-font :slant 'italic :size default-font-size)
          doom-serif-font (font-spec :family default-font :slant 'italic))
    (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" default-font default-font-size) :slant 'italic)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese-font)))
    (setq face-font-rescale-alist `((,chinese-font . ,chinese-font-rescale)))
    ))

;; (add-hook 'doom-load-theme-hook 'doom/reload-custom-font)
(add-hook 'doom-load-theme-hook (lambda() (if (display-graphic-p) (doom/reload-custom-font))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dracula)
;; doom-dracula/doom-one/doom-tomorrow-night
(setq doom-theme (if (display-graphic-p) 'doom-dracula 'doom-one))

;; fullscreen
(set-frame-parameter nil 'fullscreen 'fullboth)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; when using emacs daemon
(add-hook 'after-make-frame-functions
          (lambda (new-frame)
            (select-frame new-frame)
            (set-frame-parameter nil 'fullscreen 'fullboth)
            (if (display-graphic-p)
                (load-theme 'doom-dracula 'no-confirm)
              (progn
                (load-theme 'doom-one 'no-confirm)
                (menu-bar-mode -1)))
            ))

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
  ;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook! 'dired-mode 'all-the-icons-dired-mode)
  )

(provide 'display)

;;; display.el ends here.
