;;; font.el --- Font Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;;     字体配置，根据系统和显示器DPI动态调整。
;;; Code:

(defvar default-font-size 14 "Default font size for doom theme.")
(defvar big-font-size 18 "Big font size for doom theme.")
;; 实测现在用的字体，缩放1.25倍后对齐两个英文字母，但是中文字体显得特别大，不太美观，平时还是用1.1吧
(defvar chinese-font-rescale 1.1 "Chinese font scale ratio for doom theme.")
(defvar blamer-face-height 100 "Height for blamer face.")

;; 尝试加载 'font-spec' 。如果成功加载，则跳过动态字体配置。
(require 'font-spec nil :noerror)

(unless (featurep 'font-spec)
  (when (string= system-type "darwin")
    (setq blamer-face-height 150) ; 仅macOS使用
    (setq default-font-size 16
          big-font-size 20))

  (when (string= system-type "gnu/linux")
    ;; 根据DPI选择字体大小
    (if (>= display-pixels-per-inch 200) ; HiDPI显示器
        (setq default-font-size 34
              big-font-size 40)
      (if (>= display-pixels-per-inch 120) ; 2k
          (setq default-font-size 22
                big-font-size 28)
        (if (>= display-pixels-per-inch 96) ; FHD
            (setq default-font-size 16
                  big-font-size 20))))))

(provide 'font)

;;; font.el ends here
