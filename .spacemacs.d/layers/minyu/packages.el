;;; packages.el --- minyu layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: minyu <wmy@Gentoo-P51>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `minyu-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `minyu/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `minyu/pre-init-PACKAGE' and/or
;;   `minyu/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst minyu-packages
  '(
      (liberime-config :location (recipe
                        :fetcher gitlab
                        :repo "liberime/liberime"
                        :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))
      )
  )

(defun minyu/init-liberime-config ()
  (use-package liberime-config
    ;; :load-path "~/Workspace/gitlab/liberime/build/liberime.so"
    :config
    (defun setup-liberime ()
      ;; incase hooks not running
      (interactive)
      ;; (liberime-start "/usr/share/rime-data" "~/.emacs.d/rime/")
      ;; (liberime-select-schema "luna_pinyin_simp")))
      (liberime-select-schema "terra_pinyin")
      (add-to-list
       'pyim-schemes
       '(rime-mspy
         :document "support ;"
         :class rime
         :first-chars "abcdefghijklmnopqrstuvwxyz/"
         :rest-chars "abcdefghijklmnopqrstuvwxyz;=/,\\"
         :prefer-trigger-chars nil
         ))
      (setq pyim-default-scheme 'rime-mspy)
      ))

  )
;;; packages.el ends here
