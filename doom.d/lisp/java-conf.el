;;; java-conf.el -- Java Language Support
;;; Commentary:
;;;      java语言支持相关配置

;;; Code:

(require 'cl-lib)

(after! lsp-java
  ;; jdk8 isn't supported in latest version
  ;; https://github.com
  ;; (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")

  ;; 通过direnv控制不同项目的环境变量即可
  ;; (if (string= (getenv "EMACS_WORK_PRIORITY") "1")
  ;;   (progn
  ;;     (setq lsp-java-configuration-runtimes '[
  ;;                                             ;; (:name "JavaSE-1.8" :path "/opt/oraclejdk-bin-8")
  ;;                                             (:name "JavaSE-11" :path "/opt/openjdk-bin-11" :default t)
  ;;                                             (:name "JavaSE-17" :path "/opt/openjdk-bin-17")
  ;;                                             ])
  ;;     (setq lsp-java-configuration-maven-user-settings "~/.m2/settings-work.xml"))
  ;;   (progn
  ;;     (setq lsp-java-configuration-runtimes '[
  ;;                                             ;; (:name "JavaSE-1.8" :path "/opt/oraclejdk-bin-8")
  ;;                                             (:name "JavaSE-17" :path "/opt/openjdk-bin-11")
  ;;                                             (:name "JavaSE-11" :path "/opt/openjdk-bin-17" :default t)
  ;;                                             ])
  ;;     (setq lsp-java-configuration-maven-user-settings "~/.m2/settings-default.xml")))

  (setq lombok-library-path (concat doom-data-dir "lombok.jar"))
  (unless (file-exists-p lombok-library-path)
    (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

  (setq lsp-java-vmargs 
        '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
  (push (concat "-javaagent:"
                (expand-file-name lombok-library-path))
        lsp-java-vmargs)

  ;; Spring boot support (Experimental)
  ;; (require 'lsp-java-boot)
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  )

;; (defun java-env-oraclejdk8()
;;   "OracleJDK8 Java environment variables."
;;   (interactive)

;;   (_remove-java-home-from-path)
;;   (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
;;   (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
;;   (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
;;   )

(defun java-env-openjdk11()
  "OpenJDK11 Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/openjdk-bin-11")
  (setenv "JDK_HOME" "/opt/openjdk-bin-11")
  (setenv "PATH" (concat "/opt/openjdk-bin-11:" (getenv "PATH")))
  )

(defun java-env-opnjdk17()
  "OpenJDK17 Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/openjdk-bin-17")
  (setenv "JDK_HOME" "/opt/openjdk-bin-17")
  (setenv "PATH" (concat "/opt/openjdk-bin-17:" (getenv "PATH")))
  )

(defun maven-set-default()
  "Set Default Maven config."
  (interactive)

  (setq lsp-java-configuration-maven-user-settings "~/.m2/settings-default.xml")
  )

(defun maven-set-work()
  "Set Work Maven config."
  (interactive)

  (setq lsp-java-configuration-maven-user-settings "~/.m2/settings-work.xml")
  )

(defun java-env-switch()
  "Switch between different Java environments."
  (interactive)
  (let ((choice (completing-read
                 "Select Java version: "
                 ;; '("OracleJDK 8" "OpenJDK 11" "OpenJDK 17"))))
                 '("OpenJDK 11" "OpenJDK 17"))))
    (cond
     ;; ((string-equal choice "OracleJDK 8") (java-env-oraclejdk8))
     ((string-equal choice "OpenJDK 11") (java-env-openjdk11))
     ((string-equal choice "OpenJDK 17") (java-env-opnjdk17))
     (t (message "Invalid choice")))))

(defun maven-set-switch()
  "Switch between different Maven settings."
  (interactive)
  (let ((choice (completing-read
                 "Select Maven settings: "
                 '("Default" "Work"))))
    (cond
     ((string-equal choice "Default") (maven-set-default))
     ((string-equal choice "Work") (maven-set-work))
     (t (message "Invalid choice")))))

(defun _remove-java-home-from-path ()
  "Remove all OracleJDK or OpenJDK paths from PATH."
  (let ((path-list (split-string (getenv "PATH") ":")))
    (setenv "PATH"
            (mapconcat 'identity
                       (cl-remove-if (lambda (path)
                                       (or (string-match-p "oraclejdk" path)
                                           (string-match-p "openjdk" path)))
                                     path-list) ":"))))

(map! :leader
      (:prefix "y"
       :desc "java environment switch" :nv "j" #'java-env-switch
       :desc "maven config switch" :nv "m" #'maven-set-switch))

(provide 'java-conf)

;;; java-conf.el ends here.
