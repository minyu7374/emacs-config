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
  ;; (let ((proxy-server (format "socks5://%s:%d" ip port)))
  (let ((proxy-server (format "socks5h://%s:%d" ip port)))
       (setq url-proxy-services
             `(("no_proxy" . "^\\(localhost\\|127\\..*\\|10\\..*\\|192\\.168\\..*\\)")  ; elisp正则 \\( \\| \\. 即普通正则的( | \.
               ("http" . ,proxy-server)
               ("https" . ,proxy-server)))
       (setenv "http_proxy" proxy-server)
       (setenv "https_proxy" proxy-server)
       (setenv "ALL_PROXY" proxy-server))
       (setenv "NO_PROXY" "localhost,127.0.0.0/8,10.0.0.0/8,192.168.0.0/16")
  (setq proxy-status t)
  (message "set proxy server: %s:%d" ip port)
  )

(defun proxy-off()
  "Socks5 Proxy Off."
  (interactive)

  (setq url-gateway-method 'native)
  (setq socks-server nil)
  (setq url-proxy-services nil)
  (setenv "http_proxy" nil)
  (setenv "https_proxy" nil)
  (setenv "ALL_PROXY" nil)
  (setenv "NO_PROXY" nil)
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
