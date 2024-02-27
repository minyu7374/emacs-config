(defun default-java()
  (interactive)

  (setenv "JAVA_HOME" "/opt/openjdk-bin-11")
  (setenv "JDK_HOME" "/opt/openjdk-bin-11")
  (setenv "PATH" (concat "/opt/openjdk-bin-11:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/.m2/maven/settings.xml")
  )

(defun kanjia-java()
  (interactive)

  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/Workspace/unicom/projects/kanjia/maven/settings.xml")
  )

(map! :leader
      (:prefix ("yj" . "java env")
       :desc "default java env config" :nv "d" #'default-java
       :desc "kanjia java env config" :nv "k" #'kanjia-java
       )
      )

(provide 'diy-env)
