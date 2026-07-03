;;; java-conf.el -- Java Language Support  -*- lexical-binding: t; -*-
;;; Commentary:
;;      java 语言支持（eglot + jdtls）。
;;      - lombok 以 javaagent 注入 jdtls（jar 缺失时首次连接自动下载）。
;;      - maven user settings 走 eglot-workspace-configuration，
;;        +java/maven-switch 切换后对已连接 server 即时生效。
;;      - JDK 切换是纯环境变量操作（优先用 direnv 按项目管理，此处仅全局兜底）。
;;      启用：init.el 开启 (java +lsp +tree-sitter)，config.el 取消注释
;;      (require 'java-conf)，并确保 jdtls 可执行文件在 PATH 上。

;;; Code:

(require 'cl-lib)

;; 前向声明：eglot 延迟加载，消除字节编译的 free-function 告警。
(declare-function eglot-current-server "eglot")
(declare-function eglot-signal-didChangeConfiguration "eglot")

;;;; Server（jdtls + lombok）

;; contact 用函数以延迟求值：lombok 下载发生在首次打开 java buffer 连接时，
;; 而非配置加载时。
(set-eglot-client! '(java-mode java-ts-mode) #'+java--jdtls-contact)

(defun +java--jdtls-contact (&optional _interactive)
  "Build the jdtls command, injecting lombok and JVM tuning args."
  `("jdtls"
    ,(concat "--jvm-arg=-javaagent:" (+java--ensure-lombok))
    "--jvm-arg=-XX:+UseParallelGC"
    "--jvm-arg=-XX:GCTimeRatio=4"
    "--jvm-arg=-XX:AdaptiveSizePolicyWeight=90"
    "--jvm-arg=-Dsun.zip.disableMemoryMapping=true"
    "--jvm-arg=-Xmx4G"
    "--jvm-arg=-Xms100m"))

(defvar +java-lombok-jar (concat doom-data-dir "lombok.jar")
  "Path to the lombok jar loaded into jdtls as a javaagent.")

(defun +java--ensure-lombok ()
  "Return `+java-lombok-jar', downloading it first if missing."
  (unless (file-exists-p +java-lombok-jar)
    (url-copy-file "https://projectlombok.org/downloads/lombok.jar"
                   +java-lombok-jar))
  (expand-file-name +java-lombok-jar))

;;;; Maven settings 切换（jdtls workspace configuration）

(defvar +java-maven-settings
  '(("Default" . "~/.m2/settings-default.xml")
    ("Work"    . "~/.m2/settings-work.xml"))
  "Selectable Maven user-settings files, as (LABEL . PATH).")

(defun +java/maven-switch ()
  "Switch the Maven user settings used by jdtls."
  (interactive)
  (let* ((choice (completing-read "Select Maven settings: "
                                  (mapcar #'car +java-maven-settings) nil t))
         (path (expand-file-name (cdr (assoc choice +java-maven-settings)))))
    (setq-default eglot-workspace-configuration
                  `(:java (:configuration (:maven (:userSettings ,path)))))
    ;; 已连接的 jdtls 即时下发新配置；未连接则在下次连接时随初始化生效。
    (when-let ((server (and (fboundp 'eglot-current-server)
                            (eglot-current-server))))
      (eglot-signal-didChangeConfiguration server))
    (message "Maven settings → %s" path)))

;;;; JDK 环境切换（全局环境变量，与 LSP 无关）

(defvar +java-jdk-paths
  '(("OpenJDK 11" . "/opt/openjdk-bin-11")
    ("OpenJDK 17" . "/opt/openjdk-bin-17"))
  "Selectable JDK installations, as (LABEL . PATH).")

(defun +java/env-switch ()
  "Switch JAVA_HOME/JDK_HOME/PATH to a JDK from `+java-jdk-paths'.
切换后已连接的 jdtls 需 M-x eglot-reconnect 才会使用新 JDK。"
  (interactive)
  (let* ((choice (completing-read "Select Java version: "
                                  (mapcar #'car +java-jdk-paths) nil t))
         (path (cdr (assoc choice +java-jdk-paths))))
    (+java--strip-jdk-from-path)
    (setenv "JAVA_HOME" path)
    (setenv "JDK_HOME" path)
    (setenv "PATH" (concat path ":" (getenv "PATH")))
    (message "JAVA_HOME → %s" path)))

(defun +java--strip-jdk-from-path ()
  "Remove all OracleJDK/OpenJDK entries from PATH."
  (setenv "PATH"
          (mapconcat #'identity
                     (cl-remove-if (lambda (path)
                                     (string-match-p "oraclejdk\\|openjdk" path))
                                   (split-string (getenv "PATH") ":"))
                     ":")))

(map! :leader
      (:prefix ("yj" . "java")
       :desc "java environment switch" :nv "e" #'+java/env-switch
       :desc "maven config switch"     :nv "m" #'+java/maven-switch))

(provide 'java-conf)

;;; java-conf.el ends here.
