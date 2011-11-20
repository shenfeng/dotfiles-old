;; To use, load this in your ~/.emacs:
;; For example:
;;    (load-file "${HOME}/code/onycloud/tools/emacs/coding-style.el")

;; Indent for JavaScript, assuming you use js2-mode

(setq ruby-indent-level 2)

(setq css-indent-level 4)

(setq-default tab-width 2)

(defun map-filetype (pattern mode)
  "Map from filename patterns to major modes"
  (setq auto-mode-alist (cons `(,pattern . ,mode) auto-mode-alist)))

(map-filetype "\\.scss$" 'css-mode)
(map-filetype "\\.less$" 'css-mode)
(map-filetype "\\.js$" 'js2-mode)
(map-filetype "\\.markdown$" 'markdown-mode)
(map-filetype "\\.md$" 'markdown-mode)

;; Make Emacs use "newline-and-indent" when you hit the Enter key so
;; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key [ret] 'newline-and-indent)

(defun code-hook ()
  "Customization for writing code"
  (add-hook 'write-file-hooks 'delete-trailing-whitespace t)
  (setq show-trailing-whitespace t)
  ;; (highlight-80+-mode t)
  (setq indent-tabs-mode nil))

(add-hook 'clojure-mode-hook 'code-hook)
(add-hook 'ruby-mode-hook 'code-hook)
(add-hook 'css-mode-hook 'code-hook)
;; (add-hook 'clojure-mode-hook 'clojure-test-mode t)

(defun slime-repl-color-hook ()
  (highlight-lines-matching-regexp
   "\s+\\(rssminer\\|onyx\\|trakr\\)" "hi-green-b"))

(when (require 'rainbow-delimiters nil 'noerror)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)

(provide 'coding-style)
