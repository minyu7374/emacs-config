;;; java-conf.el -- Java Language Support
;;; Commentary:
;;;      java语言支持相关配置

;;; Code:

(require 'cl-lib)

(use-package! lsp-java
  :hook (java-mode . lsp-deferred)
  :init

  ;; jdk8 isn't supported in latest version
  ;; https://github.com/emacs-lsp/lsp-java/issues/249
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")

  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8" :path "/opt/oraclejdk-bin-8")
                                          (:name "JavaSE-17" :path "/opt/openjdk-bin-17" :default t)
                                          (:name "JavaSE-21" :path "/opt/oraclejdk-bin-21")])

  ;; 即在java项目所在同一层目录下设置maven目录，提供settings.xml文件
  (setq lsp-java-configuration-maven-user-settings "../maven/settings.xml")

  (setq lombok-library-path (concat doom-data-dir "lombok.jar"))

  (unless (file-exists-p lombok-library-path)
    (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))

  (push (concat "-javaagent:"
                (expand-file-name lombok-library-path))
        lsp-java-vmargs)

  ;; Spring boot support (Experimental)
  ;; (require 'lsp-java-boot)

  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  )

(defun java-env-opnjdk17()
  "OpenJDK17 Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/openjdk-bin-17")
  (setenv "JDK_HOME" "/opt/openjdk-bin-17")
  (setenv "PATH" (concat "/opt/openjdk-bin-17:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/.m2/maven/settings.xml")
  )

(defun java-env-oraclejdk8()
  "kanjia Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/.m2/maven/settings.xml")
  )

(defun java-env-oraclejdk21()
  "OracleJDK21 Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-21")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-21")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-21:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/.m2/maven/settings.xml")
  )

(defun java-env-kanjia()
  "kanjia Java environment variables."
  (interactive)

  (_remove-java-home-from-path)
  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/Workspace/unicom/projects/kanjia/maven/settings.xml")
  )

(defun java-env-switch()
  "Switch between different Java environments."
  (interactive)
  (let ((choice (completing-read
                 "Select Java version: "
                 '("OpenJDK 17" "OracleJDK 8" "OracleJDK 21" "kanjia"))))
    (cond
     ((string-equal choice "OpenJDK 17") (java-env-opnjdk17))
     ((string-equal choice "OracleJDK 8") (java-env-oraclejdk8))
     ((string-equal choice "OracleJDK 21") (java-env-oraclejdk21))
     ((string-equal choice "kanjia") (java-env-kanjia))
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
       :desc "java environment switch" :nv "j" #'java-env-switch))

(provide 'java-conf)

;;; java-conf.el ends here.
