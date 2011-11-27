(set-face-attribute 'default nil :font "Consolas" :height 114)
;; (set-face-attribute 'default nil :font "Inconsolata" :height 124)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete/")
(add-to-list 'load-path "~/.emacs.d/vendor/magit/")
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet/")

(require 'ac-slime)
(require 'color-theme)
(require 'auto-complete-config)
(require 'js2-mode) ;; emacs --batch -f batch-byte-compile js2-mode.el
(require 'magit)
(require 'lua-mode)
(require 'markdown-mode)
(require 'mustache-mode)
(require 'feng-anything-conf)
(require 'session)
(require 'jinja2-mode)
(require 'undo-tree)
(require 'yasnippet)
(require 'autopair)
(require 'zencoding-mode)
(require 'color-theme-zenburn)
(require 'coding-style)
(require 'feng-defuns)

(setq cua-enable-cua-keys nil) ;; don't add C-x,C-c,C-v
(cua-mode t)                   ;; for rectangles, CUA is nice
(blink-cursor-mode 1)
(color-theme-zenburn)
(column-number-mode 1)
(global-undo-tree-mode)

(setq auto-save-default nil             ; Don't want any auto saving
      kill-whole-line 1
      make-backup-files nil             ; Don't want any backup files
      yas/also-auto-indent-first-line t
      yas/snippet-dirs "~/.emacs.d/vendor/snippets"
      slime-net-coding-system 'utf-8-unix
      js2-auto-indent-p t
      ediff-split-window-function 'split-window-horizontally
      js2-indent-on-enter-key t
      js2-enter-indents-newline t)

(global-unset-key (kbd "C-x 4 ."))      ; bind it to C-c .
(global-unset-key (kbd "C-x C-h"))

(global-set-key (kbd "C-;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-M-q") 'session-jump-to-last-change)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)
(global-set-key (kbd "C-c .") 'slime-edit-definition-other-window)
(global-set-key (kbd "C-c C-l") 'copy-line)
(global-set-key (kbd "C-c C-u") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-o") 'feng-anthing-for-files)
(global-set-key (kbd "C-x C-o") 'other-window-backward)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "M-1") 'feng-select-first-window)
(global-set-key (kbd "M-2") 'feng-select-second-window)
(global-set-key (kbd "M-3") 'feng-select-third-window)
(global-set-key (kbd "M-4") 'feng-select-forth-window)
(global-set-key (kbd "M-`") 'cleanup-buffer)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-n") 'move-text-down)
(define-key global-map (kbd "C-=") 'increment-number-decimal)
(define-key global-map (kbd "C--") 'decrement-number-decimal)
(global-set-key (kbd "M-o") 'feng-anthing-for-occur)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f12] 'coding-start)
(global-set-key [f1] 'delete-other-windows)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'save-buffer)
(global-set-key [f4] 'repeat)
(global-set-key [f7] 'open-in-desktop)
(global-set-key [(insert)] nil)

;;; snippets
(yas/load-directory yas/snippet-dirs)

;;; auto-complete
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete/ac-dict")
(ac-config-default)
(setq ac-auto-show-menu 0.2
      ac-fuzzy-enable t
      ac-quick-help-height 25
      ac-menu-height 18
      ac-quick-help-delay 0.4           ;show doc quickly
      ac-use-menu-map t)
(add-to-list 'ac-modes 'slime-repl-mode)

(define-key ac-mode-map (kbd "C-c C-n") 'auto-complete)
(define-key undo-tree-map (kbd "C-.") 'undo-tree-redo)

(add-hook 'after-init-hook 'session-initialize)
(add-hook 'clojure-mode-hook 'feng-clj-mode-hook)
(add-hook 'clojure-mode-hook 'set-up-slime-ac)
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'css-mode-hook 'feng-css-mode-hook)
(add-hook 'dired-mode-hook 'feng-dired-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'feng-elisp-mode-hook)
(add-hook 'gud-mode-hook 'feng-gud-mode-hook)
(add-hook 'html-mode-hook 'feng-html-mode-hook)
(add-hook 'js2-mode-hook 'feng-js2-mode-hook)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'slime-repl-mode-hook 'slime-repl-color-hook)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'turn-on-paredit)
(add-hook 'sql-interactive-mode-hook 'no-trancate-line)
(add-hook 'python-mode-hook 'feng-python-mode-hook)
(add-hook 'c-mode-hook 'feng-c-mode-hook)

(remove-hook 'js2-mode-hook 'moz-minor-mode)

(eval-after-load 'paredit
  '(feng-paredit-key-map))

(server-start)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
