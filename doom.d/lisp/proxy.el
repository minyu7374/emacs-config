;;; proxy.el --- Proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;      Proxy 控制

;;; code:

(defvar proxy-status nil "Proxy Status (t/nil).")

(defun proxy-on()
  "Socks5 Proxy On."
  (interactive)

  (let* ((default-ip "127.0.0.1")
         (default-port 7890)
         (ip (read-string (format "proxy server IP (default %s): " default-ip) nil nil default-ip))
         (port (string-to-number (read-string (format "proxy server port (default %d): " default-port)
                                              nil nil (number-to-string default-port)))))
    (setq url-gateway-method 'socks)
    (setq socks-server `("Default server" ,ip ,port 5))
    (setq proxy-status t)
    (message "open proxy server: %s:%d" ip port))
  )

(defun proxy-off()
  "Socks5 Proxy Off."
  (interactive)

  (setq url-gateway-method 'native)
  (setq socks-server nil)
  (setq proxy-status nil)
  (message "off proxy server")
  )

(defun proxy-toggle()
  "Socks5 Proxy Toggle."
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
