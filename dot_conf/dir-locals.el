;; ((nil . ((+format-on-save-disabled-modes . (c-mode c++-mode python-mode java-mode)))))
;; .dir-locals.el 里更推荐 mapc，因为它是函数式求值语义、可直接嵌入 (eval . ...)，而 dolist 是宏，不适合这种间接求值场景。
((nil . ((eval . (mapc (lambda (mode)
                         (add-to-list '+format-on-save-disabled-modes mode))
                       '(c-mode c++-mode python-mode java-mode c-ts-mode c++-ts-mode python-ts-mode java-ts-mode))))))
