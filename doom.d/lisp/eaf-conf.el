;;; eaf-conf.el -- EAF Config
;;; Commentary:
;;      EAF 架构配置

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-music-player)
(require 'eaf-video-player)
(require 'eaf-image-viewer)
(require 'eaf-markdown-previewer)
(require 'eaf-org-previewer)
(require 'eaf-file-manager)
(require 'eaf-mindmap)
(require 'eaf-netease-cloud-music)
(require 'eaf-file-browser)
(require 'eaf-file-sender)
(require 'eaf-airshare)
(require 'eaf-markmap)
;; (require 'eaf-map)
;; (require 'eaf-rss-reader)
;; (require 'eaf-system-monitor)
;; (require 'eaf-git)

(require 'eaf-evil)
(define-key key-translation-map (kbd "SPC")
  (lambda (prompt)
    (if (derived-mode-p 'eaf-mode)
        (pcase eaf--buffer-app-name
          ("browser" (if  (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                         (kbd "SPC")
                       (kbd eaf-evil-leader-key)))
          ("pdf-viewer" (kbd eaf-evil-leader-key))
          ("image-viewer" (kbd eaf-evil-leader-key))
          (_  (kbd "SPC")))
      (kbd "SPC"))))

(setq browse-url-browser-function 'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)
(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-enable-adblocker t)
(setq eaf-browser-default-search-engine "duckduckgo")
(setq eaf-browse-blank-page-url "https://duckduckgo.com")

(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "17890")

(setq eaf-browser-dark-mode nil)
(setq eaf-pdf-dark-mode nil)
(setq eaf-image-dark-mode nil)

(provide 'eaf-conf)

;;; eaf-conf.el ends here.
