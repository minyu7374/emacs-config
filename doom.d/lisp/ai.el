;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(use-package! gptel
  :config
  (let ((token (string-trim (shell-command-to-string "pass openapi/token"))))
    (setq! gptel-api-key
           (if (or (not token) (string-empty-p token))
               (getenv "OPENAI_API_KEY") token)))
  )

(provide 'ai)

;;; ai.el ends here.
