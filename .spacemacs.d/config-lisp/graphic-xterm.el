(if (display-graphic-p)
    (progn
      ;; 设置等宽字体
      (spacemacs//set-monospaced-font "Consolas NF" "FZTieJinLiShu-S17S" 14 16)
      ;; emacs26, 改用posframe, 速度很快并且菜单不会变形，不过需要用户手动安装 posframe 包。
      (setq pyim-page-tooltip 'posframe)
)
  (progn
    (load-theme 'doom-tomorrow-night t)
    ;; 伪终端模拟器们似乎对posframe 的支持不够好
    (setq pyim-page-tooltip 'popup)
    ;; disable mouse
    (xterm-mouse-mode -1)
    (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
                 [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
                 [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
                 [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
                 [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
      (global-unset-key k)
    )
   )

  )

(provide 'graphic-xterm)
