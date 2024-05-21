;;; proxy.el --- Proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;      Proxy 控制

;;; code:

(defvar proxy-status nil "Proxy Status (t/nil).")

(defun proxy-on()
  (interactive)

  (setq url-gateway-method 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 17890 5))
  (setq proxy-status t)
  )

(defun proxy-off()
  (interactive)

  (setq url-gateway-method 'native)
  (setq socks-server nil)
  (setq proxy-status nil)
  )

(defun proxy-toggle()
  (interactive)

  (if proxy-status
      (proxy-off)
    (proxy-on))
  )

(global-set-key (kbd "\C-cyp") 'proxy-toggle)

(map! :leader
      (:prefix ("yp" . "proxy")
       :desc "proxy toggle" :nv "t" #'proxy-toggle
       :desc "proxy on" :nv "o" #'proxy-on
       :desc "proxy off" :nv "f" #'proxy-off))

(provide 'proxy)

;;; proxy.el ends here.
