;;; java-conf.el -- Java Language Support
;;; Commentary:
;;;      java语言支持相关配置

;;; Code:

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
  )

(provide 'java-conf)

;;; java-conf.el ends here.
