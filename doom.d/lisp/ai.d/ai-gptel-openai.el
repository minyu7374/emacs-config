;;; ai-gptel-openai.el --- ChatGPT subscription backend for gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;     GPTel configuration for the ChatGPT Plus/Pro OAuth backend.

;;; Code:

(declare-function gptel-make-openai-oauth "gptel-openai-oauth")
(declare-function gptel-get-backend "gptel-request")
(declare-function codex-ide--available-model-names "codex-ide-protocol")
(defvar gptel-backend)
(defvar gptel-model)
(defvar +gptel-chatgpt-models-refreshed nil)

(defun +gptel-refresh-chatgpt-models ()
  "Refresh the ChatGPT backend's model list from Codex app-server."
  (interactive)
  (let* ((backend (gptel-get-backend "ChatGPT"))
         (model-names (and (fboundp 'codex-ide--available-model-names)
                           (codex-ide--available-model-names)))
         (models (mapcar #'intern model-names)))
    (unless models
      (user-error "Unable to retrieve models from the Codex app-server"))
    (setf (gptel-backend-models backend) models)
    (when (eq gptel-backend backend)
      (unless (memq gptel-model models)
        (setq gptel-model nil)))
    (setq +gptel-chatgpt-models-refreshed t)
    (message "ChatGPT models refreshed: %s" (mapconcat #'symbol-name models ", "))))

(defun +gptel--refresh-chatgpt-models-once ()
  "Refresh ChatGPT models lazily when the first GPTel buffer opens."
  (unless +gptel-chatgpt-models-refreshed
    (condition-case err
        (+gptel-refresh-chatgpt-models)
      (error
       (message "Could not refresh ChatGPT models: %s" (error-message-string err))))))

(use-package! gptel-openai-oauth
  :after gptel
  :config
  (setq gptel-model nil
        gptel-backend
        (gptel-make-openai-oauth "ChatGPT")))

(after! gptel
  (add-hook 'gptel-mode-hook #'+gptel--refresh-chatgpt-models-once))

(provide 'ai-gptel-openai)

;;; ai-gptel-openai.el ends here.
