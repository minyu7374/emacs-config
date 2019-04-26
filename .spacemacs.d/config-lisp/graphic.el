(if (display-graphic-p)
    (progn
      ;; (load-theme 'solarized-dark t))
      (load-theme 'sanityinc-tomorrow-blue t)
      ;; 设置等宽字体
      ;; (spacemacs//set-monospaced-font "Consolas" "FZWeiBei-S03" 38 46)
      (spacemacs//set-monospaced-font "Consolas NF" "FZTieJinLiShu-S17S" 38 46)
      )
  )

(provide 'graphic)
