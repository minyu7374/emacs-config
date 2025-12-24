;;; ai-copilot.el --- Copilot conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; copilot https://github.com/copilot-emacs/copilot.el
;; accept completion from copilot and fallback to company
(use-package! copilot
  :defer t
  ;; :hook (prog-mode . copilot-mode) ;; 先不默认启动了
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("S-<tab>" . 'copilot-accept-completion-by-word)
              ("S-TAB" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  ;; :config

  ;; (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  ;; (add-to-list 'copilot-indentation-alist '(org-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(text-mode 2))
  ;; (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  )

;; 添加开关
(global-set-key (kbd "\C-cC") #'copilot-mode)
(map! :leader
      :desc "Toggle Copilot mode" "t C" #'copilot-mode)

;; (after! (evil copilot)
;;   ;; Define a custom function for handling Copilot or fallback actions
;;   (defun my/copilot-action (copilot-action fallback-action)
;;     "Perform COPILOT-ACTION if copilot-mode is active, otherwise do the default action."
;;     (if (and (bound-and-true-p copilot-mode)
;;              (copilot--overlay-visible)) ; Ensure a completion is visible
;;         (funcall copilot-action)
;;       (funcall fallback-action)))

;;   ;; Bind keys in Evil insert state globally
;;   (evil-define-key 'insert 'global
;;     (kbd "<tab>") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion #'indent-for-tab-command))
;;     (kbd "TAB") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion #'indent-for-tab-command))
;;     (kbd "S-<tab>") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion-by-word #'indent-for-tab-command))
;;     (kbd "S-TAB") (lambda () (interactive) (my/copilot-action #'copilot-accept-completion-by-word #'indent-for-tab-command))
;;     (kbd "C-n") (lambda () (interactive) (my/copilot-action #'copilot-next-completion #'evil-paste-pop-next))
;;     (kbd "C-p") (lambda () (interactive) (my/copilot-action #'copilot-previous-completion #'evil-paste-pop)))
;;   )

(provide 'ai-copilot)

;;; ai-copilot.el ends here.
