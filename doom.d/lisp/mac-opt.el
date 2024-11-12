;;; mac-opt.el -- optimization for MacOS
;;; Commentary:
;;;      针对MacOS运行效率低下问题做优化

;;; Code:

;;; 调整 Emacs GUI 选项

;; (setq mac-allow-anti-aliasing nil)              ;; 禁用字体抗锯齿，减少渲染压力
(setq frame-inhibit-implied-resize t)           ;; 禁止窗口自动调整大小
(setq inhibit-compacting-font-caches t)         ;; 禁止压缩字体缓存，减少卡顿
(setq ns-use-native-fullscreen t)               ;; 通过原生全屏模式来避免窗口切换时的重绘问题
(setq frame-resize-pixelwise t)                 ;; 更平滑的窗口缩放
(setq line-move-visual nil)                     ;; 设置为物理行移动

;;; 优化垃圾回收机制
(setq gc-cons-threshold (* 50 1000 1000))       ;; 提升GC阈值至50MB
(setq read-process-output-max (* 1024 1024))    ;; 提高进程读取输出的最大值

;;; 性能优化
;;Doom Emacs 在 macOS 上默认禁用 file-name-handler-alist 的一些处理，但可以确保你的配置文件包含以下代码，进一步降低文件系统操作的开销：
(setq file-name-handler-alist nil)

;; 在 macOS 上，使用无标题栏的模式可以提升窗口性能，Doom 中可以这样设置
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; 如果启用了 LSP，可以限制 LSP 的工作目录范围或禁用不常用的语言的 LSP 支持：
;; (setq lsp-enable-file-watchers nil)             ;; 禁用文件监视器，减少I/O

;; ;; 禁用doom启动屏幕
;; (setq +doom-dashboard-functions nil)

;; ;; 禁用 Doom Modeline 的图标和动画
;; (after! doom-modeline
;;   (setq doom-modeline-icon nil
;;         doom-modeline-major-mode-icon nil))

(provide 'mac-opt)

;;; mac-opt.el ends here.
