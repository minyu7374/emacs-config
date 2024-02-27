(defun kanjia-java()
  (interactive)

  (setenv "JAVA_HOME" "/opt/oraclejdk-bin-8")
  (setenv "JDK_HOME" "/opt/oraclejdk-bin-8")
  (setenv "PATH" (concat "/opt/oraclejdk-bin-8:" (getenv "PATH")))
  (setq lsp-java-configuration-maven-user-settings "~/Workspace/unicom/projects/kanjia/maven/settings.xml")
  )

(map! :leader
      (:prefix ("yj" . "java env")
       :desc "kanjia java env config" :nv "k" #'kanjia-java
       )
      )

(provide 'diy-env)
