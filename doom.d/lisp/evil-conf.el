;;; evil-conf.el -- Evil config -*- lexical-binding: t; -*-
;;; Commentary:
;;      Evil 配置

;;; Code:

;; 控制 * 搜索不跳转，原本的行为绑定到 g*
(after! evil
  ;; 实际实现是照抄evil-ex-search-word-forward的逻辑，只是为了不跳转将count设置为0
  (defun my/highlight-word-no-jump (&optional symbol)
    "Highlight word/symbol at point without jumping, identical logic to evil-ex-search-word-forward."
    (interactive (list evil-symbol-word-search))
    (evil-ex-start-word-search nil 'forward 0 symbol))

  (map! :n "g*" #'evil-ex-search-word-forward
        :n "*"  #'my/highlight-word-no-jump))

;; ;; 常驻eamcs模式(并不方便)
;; (after! vterm
;;   (evil-set-initial-state 'vterm-mode 'emacs))

;; 终端发送esc键 (原本的esc切换状态可以通过ctrl+g操作)
;; (map! :after vterm
;;       :map vterm-mode-map
;;       :i "<escape>" #'vterm-send-escape
;;       :i "C-c C-e" #'vterm-send-escape)
(with-eval-after-load 'vterm
  (evil-define-key 'insert vterm-mode-map
    ;; (kbd "C-c <escape>") #'vterm-send-escape
    (kbd "<escape>") #'vterm-send-escape
    (kbd "C-c C-e") #'vterm-send-escape))

(defun my/eat-send-escape ()
  "Send ESC to the eat terminal."
  (interactive)
  (eat-term-send-string eat-terminal "\e"))

(with-eval-after-load 'eat
  (evil-define-key 'insert eat-semi-char-mode-map
    (kbd "<escape>") #'my/eat-send-escape
    (kbd "C-c C-e") #'my/eat-send-escape))

(defun my/term-send-escape ()
  "Send ESC to the underlying term process."
  (interactive)
  (term-send-raw-string "\e"))

(with-eval-after-load 'term
  (evil-define-key 'insert term-raw-map
    (kbd "<escape>") #'my/term-send-escape
    (kbd "C-c C-e") #'my/term-send-escape))

(provide 'evil-conf)

;;; evil-conf.el ends here.
