;;; font.el --- Font Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;;     字体配置，根据系统和显示器DPI动态调整。
;;; Code:

(defvar default-font-size 14 "Default font size for doom theme.")
(defvar big-font-size 18 "Big font size for doom theme.")
;; 实测现在用的字体，缩放1.25倍后对齐两个英文字母，但是中文字体显得特别大，不太美观，平时还是用1.1吧
(defvar chinese-font-rescale 1.1 "Chinese font scale ratio for doom theme.")
(defvar blamer-face-height 100 "Height for blamer face.")

(defun my/setup-font-based-on-monitor (&optional frame)
  "Adjust font size according to monitor attributes for FRAME."
  ;; wayland 中 高分屏 dpi 还是96, 所以同时判断屏幕分辨率
  (interactive)
  (let* ((attrs (frame-monitor-attributes frame))
         (geom (alist-get 'geometry attrs))
         (monitor-width  (nth 2 geom))
         (monitor-height (nth 3 geom))
         (monitor-pixel-count (* monitor-width monitor-height))
         (dpi (or (bound-and-true-p display-pixels-per-inch) 96)))
    (cond
     ((or (>= monitor-pixel-count (* 3840 2160)) (>= dpi 200)) ;HiDPI
      (setq default-font-size 34 big-font-size 40))
     ((or (>= monitor-pixel-count (* 2560 1440)) (>= dpi 120)) ;2k
      (setq default-font-size 22 big-font-size 28))
     ((or (>= monitor-pixel-count (* 1920 1080)) (>= dpi 96)) ;FHD
      (setq default-font-size 16 big-font-size 20)))))

;; 尝试加载 'font-spec' 。如果成功加载，则跳过动态字体配置。
(require 'font-spec nil :noerror)

(unless (featurep 'font-spec)
  (when (eq system-type 'darwin)
    (setq blamer-face-height 150) ; 仅macOS使用
    (setq default-font-size 16
          big-font-size 20))

  (when (eq system-type 'gnu/linux)
    ;; 根据屏幕分辨率和DPI选择字体大小 (兼容 emacs daemon)
    (when (display-graphic-p) (my/setup-font-based-on-monitor))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (when (display-graphic-p frame)
                    (my/setup-font-based-on-monitor frame)))))))

(provide 'font)

;;; font.el ends here
