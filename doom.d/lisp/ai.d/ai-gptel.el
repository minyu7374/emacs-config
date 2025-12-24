;;; ai-gptel.el --- gptel conf                                -*- lexical-binding: t; -*-
;;; Commentary:
;;     AI 工具相关配置

;; Copyright (C) 2024  minyu

;; Author: minyu <minyu@outlook.com>

;;; Code:

;; gptel https://github.com/karthink/gptel
;; 用tools/llm, 改为 after!
(after! gptel
  (setq gptel-max-tokens 8192)
  (setq gptel-track-media t)

  ;;随便设置一个值，避免执行gptel时选用和api-key判空
  (setq gptel-backend (gptel-make-openai " By SetUp"))
  (setq gptel-api-key "sk-null")

  (defvar +gptel--backends nil "gptel backends can use.")
  (defvar +gptel--backend-setup-done nil)

  (defun +gptel--backend-setup ()
    "Setup backend for gptel."
    (interactive)
    (unless +gptel--backend-setup-done
      ;; (setq gptel-api-key (get-llm-api-key 'openai :token))
      (setq +gptel--backends
            `((gemini . ,(gptel-make-gemini "Gemini" :key (get-llm-api-key 'gemini :token) :stream t))

              (chatanywhere . ,(gptel-make-openai "ChatAnyWhere"
                                 :host (get-llm-api-key 'chatanywhere :host)
                                 :key (get-llm-api-key 'chatanywhere :token)
                                 ;; :header `(("Authorization" . ,(concat "Bearer " chatanywhere-token)))
                                 ;; :endpoint "/v1/chat/completions"
                                 :stream t
                                 :models '(gpt-4 gpt-4o gpt-4o-mini)))

              ;; deepseek web页面逆向api部署本地，nginx反向代理
              (minyuchat . ,(gptel-make-openai "MinyuChat"
                              :host (get-llm-api-key 'minyuchat :host)
                              :key (get-llm-api-key 'minyuchat :token)
                              :stream t
                              :models '(deepseek-chat deepseek-coder deepseek-reasoner)))))

      ;; 默认后端
      (setq gptel-backend (alist-get 'gemini +gptel--backends))
      (setq gptel-model 'gemini-2.5-flash)
      (message "gptel backend set to: %s" (gptel-backend-name gptel-backend))
      (setq +gptel--backend-setup-done t)))

  (defun +gptel--ensure-backend (&rest _args) 
    (+gptel--backend-setup))

  (advice-add  #'gptel :before #'+gptel--ensure-backend) ;; 明确指定gptel函数引用
  (advice-add 'gptel-menu :before #'+gptel--ensure-backend)
  (advice-add 'gptel-rewrite :before #'+gptel--ensure-backend))

(global-set-key (kbd "\C-cg") 'gptel)
(map! :leader
      (:prefix ("yg" . "gptel")
       :desc "gptel backend setup"         :nv "b" #'+gptel--backend-setup
       :desc "Create new chat buffer"      :nv "g" #'gptel
       :desc "Menu for chat preferences"   :nv "m" #'gptel-menu
       :desc "Rewrite/refactor selected region"         :nv "w" #'gptel-rewrite
       :desc "Add/remove region/buffer to chat context" :nv "d" #'gptel-add
       :desc "Add a file to chat context"  :nv "f" #'gptel-add-file
       :desc "Stop active gptel process"   :nv "k" #'gptel-abort))

(provide 'ai-gptel)

;;; ai-gptel.el ends here.
