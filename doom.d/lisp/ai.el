;;; ai.el --- AI conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(add-to-list 'load-path (expand-file-name "ai.d" (file-name-directory (or load-file-name buffer-file-name))))

(require 'ai-common)
(require 'ai-gptel)
(require 'ai-copilot)
;; (require 'ai-aider)
(require 'ai-claude)

(provide 'ai)
;;; ai.el ends here.
