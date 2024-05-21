;;; diy-env.el -- DIY Environment Variables Config
;;; Commentary:
;;      自定义的一些环境变量配置

;;; Code:
(defun default-java()
  "Default Java environment variables."
  (interactive)

  (setenv "JAVA_HOME" "/opt/openjdk-bin-17")
  (setenv "JDK_HOME" "/opt/openjdk-bin-17")
  (setenv "PATH" (concat "/opt/openjdk-bin-17:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/.m2/maven/settings.xml")
  )

(defun kanjia-java()
  "Kanjia Java environment variables."
  (interactive)

  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/Workspace/unicom/projects/kanjia/maven/settings.xml")
  )

(map! :leader
      (:prefix ("yj" . "java env")
       :desc "default java env config" :nv "d" #'default-java
       :desc "kanjia java env config" :nv "k" #'kanjia-java))

(provide 'diy-env)

;;; diy-env.el ends here.
