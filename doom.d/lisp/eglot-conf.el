;;; eglot-conf.el -- Eglot Config  -*- lexical-binding: t; -*-
;;; Commentary:
;;      Eglot（LSP）配置，配合 init.el 的 (lsp +eglot)。
;;      - 每个 major-mode 只挂一个 server（eglot-server-programs 首个匹配）；
;;        换 server：改注册后 M-x eglot-reconnect。
;;      - 悬浮文档用 eldoc + eldoc-box（childframe）。
;;      - 诊断直通 flymake（checkers/syntax +flymake），键位见 syntax-check.el。
;;
;;      结构：server 注册 → 悬浮文档(eldoc-box) 与命令 → 键位。

;;; Code:

;;;; Server

;; python 用 basedpyright（+eglot 下 +pyright flag 不装 server，需显式注册）。
(set-eglot-client! '(python-mode python-ts-mode)
                   '("basedpyright-langserver" "--stdio"))

;; 光标静止多久后弹文档。eldoc 属核心、启动即可用，顶层 setq 即可。
(setq eldoc-idle-delay 0.5)

;; signatureHelp 与 hover 的显示去重（不改 eglot 的 compose 合并策略）：
;; 两者同屏时内容重复（签名+摘要 vs 完整文档），在显示端按 doc 的 :origin
;; 过滤——hover 在场就丢弃 signature 那份；hover 缺席（如光标在参数位，
;; 该处 hover 通常为空）signature 照常显示，参数高亮不丢。
;; 其它来源（flymake 诊断、code-action 提示等）原样透传，不受影响。
(defun +eglot--prefer-hover-a (args)
  "Drop the signatureHelp doc from ARGS when a hover doc is also present.
ARGS is (DOCS INTERACTIVE) as passed to `eldoc-display-functions' members;
each DOC carries the `eldoc-documentation-functions' member that produced
it in its :origin property."
  (let ((docs (car args)))
    (if (and (seq-find (+eglot--doc-from-p 'eglot-hover-eldoc-function) docs)
             (seq-find (+eglot--doc-from-p 'eglot-signature-eldoc-function) docs))
        (cons (seq-remove (+eglot--doc-from-p 'eglot-signature-eldoc-function) docs)
              (cdr args))
      args)))

(defun +eglot--doc-from-p (fn)
  "Return a predicate matching eldoc docs whose :origin is FN."
  (lambda (doc) (eq (plist-get (cdr doc) :origin) fn)))

;; 挂到所有会展示 docs 的显示端：echo area / *eldoc* buffer / eldoc-box childframe。
(dolist (fn '(eldoc-display-in-echo-area eldoc-display-in-buffer))
  (advice-add fn :filter-args #'+eglot--prefer-hover-a))
(after! eldoc-box
  (advice-add 'eldoc-box--eldoc-display-function
              :filter-args #'+eglot--prefer-hover-a))

;;;; 悬浮文档（eldoc-box）

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
  ;; childframe 弹在光标处；hover-mode 默认弹右上角。
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

;;;; 键位

;; Doom 已在 code leader (SPC c) 下为 eglot 绑好大部分命令，无需重复：
;;   SPC c a  code action    SPC c r  rename        SPC c j  find declaration / 符号
;;   SPC c d/D  定义 / 引用    SPC c k  文档(即 K)     SPC c i  implementations
;;   SPC c t  类型定义         SPC c x  诊断 / 错误列表
;; 悬浮文档/杂项走自定义子前缀，两套等价方案并存（皆为有意设计，勿删任一）：
;; SPC c l（leader）与原始 C-c l（无 leader、insert state 也能按）。两处相同：
;;   k/K 当前buffer/全局 开关悬浮 · e 错误列表 · i imenu · R 重连
;; （rename/action 直接用 Doom 的 SPC c r / SPC c a，不再重复绑。）

;; gk：光标处弹文档，顶层全局。
(map! :desc "Show documentation" :nv "gk" #'+eglot/show-doc-at-point)

;; SPC c l 子前缀（doom leader）。
(map! :leader
      (:prefix ("c" . "code")
               (:prefix ("l" . "eglot")
                :desc "Toggle doc hover"       "k" #'eldoc-box-hover-mode
                :desc "Toggle doc hover (all)" "K" #'+eglot/toggle-doc-hover-globally
                :desc "List errors"            "e" #'flymake-show-buffer-diagnostics
                :desc "Imenu"                  "i" #'consult-imenu
                :desc "Reconnect"              "R" #'eglot-reconnect)))

;; C-c l 原始前缀（无 leader，insert state 可用）。
(map! "C-c l k" #'eldoc-box-hover-mode
      "C-c l K" #'+eglot/toggle-doc-hover-globally
      "C-c l e" #'flymake-show-buffer-diagnostics
      "C-c l i" #'consult-imenu
      "C-c l R" #'eglot-reconnect)

(provide 'eglot-conf)

;;; eglot-conf.el ends here.
