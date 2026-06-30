;;; eglot-conf.el -- Eglot Config  -*- lexical-binding: t; -*-
;;; Commentary:
;;      Eglot 方案（lsp-conf 的对应实现）。
;;      在 init.el 中启用 (lsp +eglot +peek) 时由 config.el 选择加载本文件；
;;      与之配套，treemacs 不再带 +lsp（lsp-treemacs 是 lsp-mode 专属）。
;;
;;      与 lsp-mode 的差异：
;;      - eglot 每个 major-mode 只挂一个 server（eglot-server-programs 首个匹配），
;;        不存在 dual diagnostics，无需 lsp-enabled/disabled-clients。
;;      - lsp-ui 不可用，悬浮文档改用 eldoc + eldoc-box（childframe）。
;;      - 无 +lsp/switch-client 等价物；换 server 改 eglot-server-programs 后
;;        M-x eglot-reconnect。
;;      - flycheck：checkers 为 (syntax +childframe)（非 -flymake）时，Doom 会装
;;        flycheck-eglot 并挂到 eglot-managed-mode。
;;
;;      结构：① server 注册 → ② 悬浮文档(eldoc-box) 与命令 → ③ 键位(全部在 after! 内)。

;;; Code:

;;;; ① Server

;; python 用 basedpyright（对应 lsp-conf 的 lsp-pyright-langserver-command）。
;; +pyright flag 在 +eglot 下不装 lsp-pyright，需在此显式注册。
(set-eglot-client! '(python-mode python-ts-mode)
                   '("basedpyright-langserver" "--stdio"))

;; 光标静止多久后弹文档（对应 lsp-ui-doc-delay）。eldoc 属核心、启动即可用，顶层 setq 即可。
(setq eldoc-idle-delay 0.5)

;;;; ② 悬浮文档（eldoc-box）

;; 是否在 eglot buffer 自动开启光标处悬浮文档：k 只切当前 buffer，K 切它并即时应用到
;; 所有 eglot buffer（取代旧的 global minor mode——那种写法开启时会污染全部 buffer、
;; 关闭又对新 buffer 不生效）。
(defvar +eglot-doc-hover t
  "When non-nil, auto-enable at-point documentation in eglot buffers.")

;; 前向声明：命令里会读 eldoc-box 的悬浮开关状态与内部清理定时器，
;; 提前 declare 以消除延迟加载下字节编译的 free-variable 告警。
(defvar eldoc-box-hover-mode)
(defvar eldoc-box--cleanup-timer)

(use-package! eldoc-box
  :after eglot
  :config
  ;; childframe 弹在光标处（对应 lsp-ui-doc-position 'at-point）；hover-mode 默认弹右上角。
  (setq eldoc-box-position-function eldoc-box-at-point-position-function)

  ;; 逐 buffer 自动开启，受 +eglot-doc-hover 控制。
  ;; 用 eldoc-box-hover-mode 而非 hover-at-point-mode：后者往 post-command-hook 挂
  ;; follow-cursor，对“移动光标”这类非输入命令会 inhibit 0.5s，与 eldoc idle + eglot
  ;; 异步 hover 抢跑，导致停在符号上大概率不弹；hover-mode 无此抑制，静止即稳定显示。
  (add-hook! 'eglot-managed-mode-hook
    (defun +eglot-init-doc-hover-h ()
      (eldoc-box-hover-mode
       (if (and +eglot-doc-hover (bound-and-true-p eglot--managed-mode)) 1 -1)))))

(defun +eglot/show-doc-at-point ()
  "Pop the documentation childframe for the symbol at point.
Works whether or not the auto hover (`eldoc-box-hover-mode') is on."
  (interactive)
  (if (bound-and-true-p eldoc-box-hover-mode)
      ;; 悬浮已开：走同一条显示路径，childframe 由 hover 逻辑维护、不会被误清。
      (eldoc t)
    ;; 悬浮已关：其残留的全局清理定时器（每秒自续）会立刻收掉 help-at-point 弹出的
    ;; childframe，先取消再弹（下次悬浮显示时定时器自动重建，不影响自动悬浮）。
    (when (timerp (bound-and-true-p eldoc-box--cleanup-timer))
      (cancel-timer eldoc-box--cleanup-timer)
      (setq eldoc-box--cleanup-timer nil))
    (eldoc-box-help-at-point)))

(defun +eglot/toggle-doc-hover-globally ()
  "Toggle at-point doc hover for all current and future eglot buffers."
  (interactive)
  (setq +eglot-doc-hover (not +eglot-doc-hover))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p eglot--managed-mode)
        (eldoc-box-hover-mode (if +eglot-doc-hover 1 -1)))))
  (message "Eglot doc hover globally %s" (if +eglot-doc-hover "enabled" "disabled")))

;;;; ③ 键位

;; Doom 已在 code leader (SPC c) 下为 eglot 绑好大部分命令，无需重复：
;;   SPC c a  code action    SPC c r  rename        SPC c j  find declaration / 符号
;;   SPC c d/D  定义 / 引用    SPC c k  文档(即 K)     SPC c i  implementations
;;   SPC c t  类型定义         SPC c x  诊断 / 错误列表
;; 缺的是 lsp-mode 时代 SPC c l（= lsp-command-map）那一档——eglot 下没有该前缀
;; （SPC c l 只在 :tools lsp -eglot 时绑到 lsp-command-map）。这里沿用 SPC c l 作为
;; eglot 的悬浮文档/杂项子前缀，并保留原始 C-c l（无 leader、insert 也能按）。两处相同：
;;   k/K 当前buffer/全局 开关悬浮 · e 错误列表 · i imenu · R 重连
;; （rename/action 直接用 Doom 的 SPC c r / SPC c a，不再重复绑。）

;; gk：光标处弹文档（对应 lsp-conf gk → lsp-ui-doc-show），顶层全局。
(map! :desc "Show documentation" :nv "gk" #'+eglot/show-doc-at-point)

;; SPC c l 子前缀（doom leader，沿用 lsp 时代 SPC c l = lsp-command-map 的习惯）。
(map! :leader
      (:prefix ("c" . "code")
       (:prefix ("l" . "eglot")
        :desc "Toggle doc hover"       "k" #'eldoc-box-hover-mode
        :desc "Toggle doc hover (all)" "K" #'+eglot/toggle-doc-hover-globally
        :desc "List errors"            "e" #'flycheck-list-errors
        :desc "Imenu"                  "i" #'consult-imenu
        :desc "Reconnect"              "R" #'eglot-reconnect)))

;; C-c l 原始前缀（沿用 lsp-conf 的 global-set-key 习惯）。
(map! "C-c l k" #'eldoc-box-hover-mode
      "C-c l K" #'+eglot/toggle-doc-hover-globally
      "C-c l e" #'flycheck-list-errors
      "C-c l i" #'consult-imenu
      "C-c l R" #'eglot-reconnect)

(provide 'eglot-conf)

;;; eglot-conf.el ends here.
