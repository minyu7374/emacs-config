;;; protobuf-conf.el -- Protobuf Config  -*- lexical-binding: t; -*-
;;; Commentary:
;;      Protobuf 语言配置：protobuf-ts-mode（tree-sitter）+ buf 工具链。
;;; Code:

;; protobuf-ts-mode 的 autoload 已把 .proto 挂到 auto-mode-alist，这里显式声明以防万一。
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-ts-mode))

;; buf 内置 LSP：跳转/补全/hover/诊断。一并注册 protobuf-mode 作为无 grammar 时的回退键。
(set-eglot-client! '(protobuf-ts-mode protobuf-mode) '("buf" "lsp" "serve"))

(provide 'protobuf-conf)

;;; protobuf-conf.el ends here.
