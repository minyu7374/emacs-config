;;; proxy.el --- Proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;      Proxy 控制

;;; code:

(defvar proxy-status nil "Proxy Status (t/nil).")
(defvar default-proxy-ip "127.0.0.1" "Default Proxy Server IP.")
(defvar default-proxy-port 7890 "Default Proxy Server Port.")

(defun proxy-on()
  "Proxy On with Custom IP and Port."
  (interactive)

  (let* ((ip (read-string (format "proxy server IP (default %s): " default-proxy-ip) nil nil default-proxy-ip))
         (port (string-to-number (read-string (format "proxy server port (default %d): " default-proxy-port)
                                              nil nil (number-to-string default-proxy-port))))
         ;;(use-socks (y-or-n-p "Use socks proxy(y/n)?")))
         (use-socks-str (read-string "Use SOCKS proxy(y/N)? " nil nil "N"))
         (use-socks (string-equal (downcase use-socks-str) "y")))

    (_proxy-on ip port use-socks))
  )

(defun _proxy-on(ip port use-socks)
  (if use-socks
      (progn
        (setq url-gateway-method 'socks)
        (setq socks-server `("Default server" ,ip ,port 5))
        (message "set socks proxy server: %s:%d" ip port))
    (progn
      (setq url-gateway-method 'native)
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|127\\..*\\|10\\..*\\|192\\.168\\..*\\)")  ; elisp正则 \\( \\| \\. 即普通正则的( | \.
              ("http" . ,(format "%s:%d" ip port))))
      (message "set http proxy server: %s:%d" ip port)))
  (setq proxy-status t)
  )

(defun proxy-off()
  "Proxy Off."
  (interactive)

  (setq url-gateway-method 'native)
  (setq socks-server nil)
  (setq url-proxy-services nil)
  (setq proxy-status nil)
  (message "off proxy server")
  )

(defun proxy-toggle()
  "Proxy Toggle."
  (interactive)

  (if proxy-status
      (proxy-off)
    (proxy-on))
  )

;; 现在使用chatgpt，启动emacs就按默认值开启代理
(_proxy-on default-proxy-ip default-proxy-port nil)

(global-set-key (kbd "\C-cyp") 'proxy-toggle)

(map! :leader
      (:prefix ("yp" . "proxy")
       :desc "proxy toggle" :nv "t" #'proxy-toggle
       :desc "proxy on" :nv "o" #'proxy-on
       :desc "proxy off" :nv "f" #'proxy-off))

(provide 'proxy)

;;; proxy.el ends here.
