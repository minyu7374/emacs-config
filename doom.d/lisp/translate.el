;;; translate.el --- 翻译工具配置                    -*- lexical-binding: t; -*-

;; Copyright (C) 2026  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(use-package! gt
  ;; :ensure t
  ;; :custom
  ;; (gt-langs '(en zh))

  :config
  ;; 指定客户端和代理
  ;; (setq gt-http-backend (pdd-curl-backend))
  ;; (setq gt-http-proxy "socks5://127.0.0.1:7890")
  ;; 配置为动态决定使用什么客户端，走什么代理
  (setq gt-http-proxy
        (lambda (request) ; 配置只有符合条件的网站走代理，其他的不走
          (when (string-match-p "\\(google.*\\|deepl\\|openai\\|bing\\)\\.com" (oref request url))
            "socks5://127.0.0.1:7890")))

  (setq gt-preset-translators
        `((ts-posframe . ,(gt-translator
                           :taker (gt-taker :langs '(en zh) :text 'word)
                           :engines (list (gt-google-engine :parse (gt-google-summary-parser)) ;; 弹窗，压缩展示内容
                                          (gt-youdao-dict-engine :if 'word)
                                          (gt-youdao-suggest-engine :if 'word))
                           :render (gt-posframe-pop-render)))
          (ts-overlay . ,(gt-translator
                          :taker (gt-taker :langs '(en zh) :text 'word)
                          :engines (list (gt-youdao-dict-engine :if 'word)
                                         (gt-bing-engine)
                                         (gt-google-engine)
                                         (gt-youdao-suggest-engine :if 'word))
                          :render (gt-overlay-render)))
          (ts-word . ,(gt-translator
                       :taker (gt-taker :langs '(en zh) :text 'word)
                       :engines (list (gt-youdao-dict-engine :if 'word)
                                      (gt-bing-engine)
                                      (gt-google-engine)
                                      (gt-youdao-suggest-engine :if 'word))
                       :render (gt-buffer-render)))
          (ts-buffer . ,(gt-translator
                         :taker (gt-taker :langs '(en zh) :text 'buffer)
                         :engines (list
                                   (gt-bing-engine)
                                   (gt-google-engine))
                         :render (gt-buffer-render)))))

  ;; 原本只有鼠标退出，这里添加q/esc退出
  (map! :map gt-overlay-render-map
        "q"      #'gt-delete-render-overlays
        "ESC"    #'gt-delete-render-overlays
        "<escape>" #'gt-delete-render-overlays)

  (let ((trans (if (display-graphic-p) 'ts-posframe 'ts-overlay)))
    (setq gt-default-translator (alist-get trans gt-preset-translators))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (let ((trans (if (display-graphic-p frame) 'ts-posframe 'ts-overlay)))
                (setq gt-default-translator (alist-get trans gt-preset-translators))))))

(global-set-key (kbd "\C-ctt") 'gt-translate)
(map! :leader
      (:prefix ("yt" . "Translate by gt.el")
       :desc "Translate Run" :nv "t" #'gt-translate
       :desc "Switch Translator" :nv "s" #'gt-switch-translator
       :desc "Translate Setup" :nv "m" #'gt-setup
       :desc "Quit Overlay Render" :nv "q" #'gt-delete-render-overlays))

(provide 'translate)

;;; translate.el ends here.
