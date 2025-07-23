;;; git.el -- Git Config
;;; Commentary:
;;;     git配置
;;; code:

(use-package! blamer
  :bind (("C-c b" . blamer-show-commit-info)
         ("C-c B" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 50)
  (blamer-author-formatter " ✎ %s ")
  (blamer-datetime-formatter "[%s]")
  (blamer-commit-formatter " ● %s")

  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 100
                   :italic t)))
  :config
  (global-blamer-mode 0))

(map! :leader (:prefix "t"
               ;;:desc "Toggle git blame (blamer)" "B" #'blamer-mode))
               :desc "Toggle git blame (blamer)" "B" #'global-blamer-mode))

(provide 'git)

;;; git.el ends here.
