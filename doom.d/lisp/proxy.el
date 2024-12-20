;;; proxy.el --- Proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;      Proxy 控制

;;; code:

(defvar proxy-status nil "Proxy Status (t/nil).")
(defvar default-proxy-ip "127.0.0.1" "Default Socks5 Proxy IP.")
(defvar default-proxy-port 7890 "Default Socks5 Proxy Port.")

(defun proxy-on()
  "Socks5 Proxy On with Custom IP and Port."
  (interactive)

  (let ((ip (read-string (format "proxy server IP (default %s): " default-proxy-ip) nil nil default-proxy-ip))
        (port (string-to-number (read-string (format "proxy server port (default %d): " default-proxy-port)
                                             nil nil (number-to-string default-proxy-port)))))
    (_proxy-on ip port))
  )

(defun _proxy-on(ip port)
  (setq url-gateway-method 'socks)
  (setq socks-server `("Default server" ,ip ,port 5))
  (setq proxy-status t)
  (message "set proxy server: %s:%d" ip port)
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

;; 现在使用chatgpt，启动emacs就按默认值开启代理
(_proxy-on default-proxy-ip default-proxy-port)

(global-set-key (kbd "\C-cyp") 'proxy-toggle)

(map! :leader
      (:prefix ("yp" . "proxy")
       :desc "proxy toggle" :nv "t" #'proxy-toggle
       :desc "proxy on" :nv "o" #'proxy-on
       :desc "proxy off" :nv "f" #'proxy-off))

(provide 'proxy)

;;; proxy.el ends here.
