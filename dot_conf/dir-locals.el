;; ((nil . ((+format-on-save-disabled-modes . (c-mode c++-mode python-mode java-mode)))))
((nil . ((eval . (mapc (lambda (mode)
                         (add-to-list '+format-on-save-disabled-modes mode))
                       '(c-mode c++-mode python-mode java-mode))))))
