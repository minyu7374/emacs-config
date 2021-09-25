;;; init-auto-insert.el --- Auto insert file header.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/hondotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Basic configurations.
;;

;;; Code:
;; modify by minyu

(use-package! autoinsert
  :hook (prog-mode . auto-insert-mode)
  :config
  (defun insert-string(&optional prefix)
    (replace-regexp-in-string
     "^" (or prefix comment-start)
     (concat
      (make-string 80 ?*) "\n"
      "Copyright © " (substring (current-time-string) -4) " " (user-full-name) "\n"
      "File Name: " (file-name-nondirectory buffer-file-name) "\n"
      "Author: " (user-full-name)"\n"
      (format "Email:  %s\n" user-mail-address)
      "Created: " (format-time-string "%Y-%m-%d %T (%Z)" (current-time)) "\n"
      ;; "Last Update: \n"
      ;; "         By: \n"
      "Description: \n"
      (make-string 80 ?*))))

  (setq auto-insert-query 'ask
        auto-insert-alist
        '(((ruby-mode . "Ruby program") nil
           "#!/usr/bin/env ruby\n"
           "# -*- encoding: utf-8 -*-\n"
           (insert-string) "\n")
          ((python-mode . "Python program") nil
           "#!/usr/bin/env python\n"
           "# -*- coding: utf-8 -*-\n"
           (insert-string) "\n")
          ((c-mode . "C program") nil
           "/*"
           (string-trim-left (insert-string " ")) "*/\n"
           "#include<stdio.h>\n"
           "#include<string.h>\n")
          ((sh-mode . "Shell script") nil
           "#!/bin/bash\n"
           (insert-string) "\n")
          ((go-mode . "Go program") nil
           "/*"
           (string-trim-left (insert-string " ")) "*/\n"))))

(provide 'auto-insert-header)

;;; init-auto-insert.el ends here
