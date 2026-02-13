;;; ai-gptel.el --- gptel conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

(defvar +gptel--backends nil "Gptel backends list.")
(defvar +gptel--active-backend nil "Current Active gptel backend.")
(defvar +gptel--backend-setup-done nil)
(add-hook '+llm-cache-clearers (lambda () (setq +gptel--backend-setup-done nil)) 'append)

(defun +gptel--backend-setup (&optional backend)
  "Setup backend for gptel."
  (interactive)
  (unless +gptel--backend-setup-done
    (setq +gptel--backends
          `((gemini . (,(gptel-make-gemini "Gemini" :key (+llm-get-provider-conf 'gemini :token) :stream t) gemini-2.5-flash))

            (nvidia . (,(gptel-make-openai "Nvidia"
                          :host (+llm-get-provider-conf 'nvidia :host)
                          :key (+llm-get-provider-conf 'nvidia :token)
                          :models '(z-ai/glm4.7 minimaxai/minimax-m2.1)
                          :stream t) minimaxai/minimax-m2.1))

            (zai . (,(gptel-make-openai "Zai"
                       :host (+llm-get-provider-conf 'zai :host)
                       :key (+llm-get-provider-conf 'zai :token)
                       :endpoint "/chat/completions"
                       :models '(glm-5 glm-4.7 glm-4.6v glm-4.5 glm-4.5-air)
                       :stream t) glm-4.5-air))

            (chatanywhere . (,(gptel-make-openai "ChatAnyWhere"
                                :host (+llm-get-provider-conf 'chatanywhere :host)
                                :key (+llm-get-provider-conf 'chatanywhere :token)
                                :models '(gpt-5.1 gpt-5-mini gpt-4 gpt-4o gpt-4o-mini)
                                :stream t) gpt-5-mini))

            ;; deepseek web页面逆向api部署本地，nginx反向代理
            (minyuchat . (,(gptel-make-openai "MinyuChat"
                             :host (+llm-get-provider-conf 'minyuchat :host)
                             :key (+llm-get-provider-conf 'minyuchat :token)
                             :models '(deepseek-chat deepseek-coder deepseek-reasoner)
                             :stream t) deepseek-chat))))

    (+gptel--switch-backend (or backend +gptel--active-backend (caar +gptel--backends)))
    (setq +gptel--backend-setup-done t)))

(defun +gptel--switch-backend (backend)
  "Switch gptel backend to BACKEND."
  (interactive
   (progn
     (unless +gptel--backend-setup-done (+gptel--backend-setup))
     (list (intern (completing-read "Select backend: " (mapcar #'car +gptel--backends) nil t)))))
  (setq +gptel--active-backend backend)
  (let ((backend-obj (alist-get +gptel--active-backend +gptel--backends)))
    (when backend-obj
      (setq gptel-backend (car backend-obj))
      (setq gptel-model (cadr backend-obj))
      (message "gptel backend switched to: %s" (gptel-backend-name gptel-backend)))))

;; gptel https://github.com/karthink/gptel
;; 用tools/llm, 改为 after!
(after! gptel
  (setq gptel-max-tokens 8192)
  (setq gptel-track-media t)
  (+gptel--backend-setup))

(global-set-key (kbd "\C-cg") 'gptel)
(map! :leader
      (:prefix ("yg" . "gptel")
       :desc "Create new chat buffer"      :nv "g" #'gptel
       :desc "gptel backend setup"         :nv "b" #'+gptel--backend-setup
       :desc "Switch gptel backend"        :nv "s" #'+gptel--switch-backend
       :desc "Menu for chat preferences"   :nv "m" #'gptel-menu
       :desc "Rewrite/refactor selected region"         :nv "w" #'gptel-rewrite
       :desc "Add/remove region/buffer to chat context" :nv "d" #'gptel-add
       :desc "Add a file to chat context"  :nv "f" #'gptel-add-file
       :desc "Stop active gptel process"   :nv "k" #'gptel-abort))

(provide 'ai-gptel)

;;; ai-gptel.el ends here.
