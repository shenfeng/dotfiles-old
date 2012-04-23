(require 'feng-defuns)

(defun feng-elisp-mode-hook ()
  (setq mode-name "el")
  (define-key emacs-lisp-mode-map [f9] 'edebug-defun)
  (define-key emacs-lisp-mode-map (kbd "C-M-q") 'session-jump-to-last-change)  )

(defun feng-paredit-key-map ()
  (define-key paredit-mode-map (kbd "M-<up>") 'beginning-of-defun)
  (define-key paredit-mode-map (kbd "M-<down>") 'end-of-defun)
  (define-key paredit-mode-map (kbd "M-q") 'nil)
  ;; include forward
  (define-key paredit-mode-map (kbd "C-M-0") 'paredit-forward-slurp-sexp)
  ;; exclude forward
  (define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
  ;; include backward
  (define-key paredit-mode-map (kbd "C-M-9") 'paredit-backward-slurp-sexp)
  ;; exclude backward
  (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp))

(defun feng-magic-key-map ()
  (define-key magit-mode-map (kbd "M-1") nil)
  (define-key magit-mode-map (kbd "M-2") nil)
  (define-key magit-mode-map (kbd "M-3") nil)
  (define-key magit-mode-map (kbd "M-4") nil))

(defun feng-html-mode-hook ()
  (define-key html-mode-map (kbd "<M-left>") 'sgml-skip-tag-backward)
  (define-key html-mode-map (kbd "<M-right>") 'sgml-skip-tag-forward)
  (define-key html-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key html-mode-map (kbd "M-<down>") 'move-text-down)
  (yas/minor-mode)
  (hl-line-mode))

(defun feng-python-mode-hook ()
  (make-local-variable 'ac-ignores)
  (autopair-mode)
  (require 'pymacs)
  (setq ropemacs-enable-shortcuts nil)
  (pymacs-load "ropemacs" "rope-")
  (setq autopair-blink nil)
  (idle-highlight-mode t)
  (yas/minor-mode)
  (define-key py-mode-map (kbd "C-c .") 'rope-goto-definition-other-window)
  (define-key py-mode-map (kbd "M-.") 'rope-goto-definition)
  (define-key py-mode-map (kbd "M-,") 'rope-pop-definition-stack)
  (setq ac-ignores '("if" "for"))
  (setq mode-name "py"))

;;; http://xahlee.org/emacs/file_management.html
(defun feng-dired-mode-hook ()
  ;; was dired-advertised-find-file
  (define-key dired-mode-map [ret] 'dired-find-alternate-file)
  (define-key dired-mode-map [f2] 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd "r") 'dired-do-rename)
  (define-key dired-mode-map (kbd "c") 'dired-do-copy)
  (define-key dired-mode-map (kbd ".") 'dired-next-dirline)
  (define-key dired-mode-map (kbd ",") 'dired-prev-dirline)
  (define-key dired-mode-map (kbd "M-m") 'dired-mark-files-regexp)
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file ".."))))

(defun feng-ibuffer-mode-hook ()
  (define-key ibuffer-mode-map (kbd "U") 'ibuffer-unmark-all))



(defun feng-js2-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq js2-basic-offset 2)
  (hl-line-mode)
  (setq mode-name "js2")
  (define-key js2-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key js2-mode-map (kbd "M-<down>") 'move-text-down)
  (setq ac-ignores '("log" "tc" "df" "fc" "el" "ei" "if" "ife" "for"))
  (yas/minor-mode)
  (autopair-mode)
  (setq autopair-blink nil)
  (font-lock-add-keywords
   'nil `(("\\(function *\\)("
           (0 (progn (compose-region (match-beginning 1)
                                     (match-end 1) "ƒ")
                     nil))))))

(defun feng-c-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq ac-ignores '("in" "for" "if" "def"))
  (define-key c-mode-map (kbd "C-c C-l") 'copy-line)
  (define-key c-mode-map (kbd "M-q") 'cleanup-buffer)
  (define-key c-mode-map [f9] 'gud-break)
  (define-key c-mode-map [f6] 'gud-next)
  (define-key c-mode-map [f5] 'gud-step)
  (idle-highlight-mode t)
  (setq comment-start "// "
        comment-end "")
  (hl-line-mode)
  (autopair-mode)
  (setq autopair-blink nil)
  (yas/minor-mode))

(defun feng-gud-mode-hook ()
  (define-key gud-mode-map [f9] 'gud-break)
  (define-key gud-mode-map [f5] 'gud-step)    ; step into
  (define-key gud-mode-map [f6] 'gud-next)    ; step
  (define-key gud-mode-map [f7] 'gud-finish)) ; step return

(defun feng-clj-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq mode-name "clj")
  (let ((ns (clojure-find-package)))    ; defined in clojure-mode.el
    (when (search "-test" ns)
      (save-window-excursion
        (clojure-test-mode t)
        (define-key clojure-test-mode-map [f9] 'clojure-test-run-tests))))
  (define-key clojure-mode-map (kbd "C-M-q") 'session-jump-to-last-change)
  (define-key clojure-mode-map (kbd "C-c .")
    'slime-edit-definition-other-window)
  (setq ac-ignores '("ns" "df" "dfp" "dt" "ns" "ss" "resp" "bp"))
  (yas/minor-mode))

(defun feng-css-mode-hook ()
  (make-local-variable 'ac-ignores)
  (define-key css-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key css-mode-map (kbd "M-<down>") 'move-text-down)
  (setq ac-ignores '("bg" "bgc" "ff" "fl" "fr" "fs" "fw" "lh" "pa" "pr" "ta"
                     "td" "va" "hi"))
  (yas/minor-mode)
  (hl-line-mode)
  (font-lock-add-keywords
   nil '(("\$[^\s:;]+" . font-lock-constant-face)
         ("//.*$" . font-lock-comment-face)
         ("#[abcdef[:digit:]]\\{6\\}"
          (0 (put-text-property
              (match-beginning 0)
              (match-end 0)
              'face (list :background
                          (match-string-no-properties 0))))))))

(defun feng-go-mode-hook ()
  (autopair-mode))

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
(add-hook 'after-change-functions 'feng-buffer-change-hook)
(add-hook 'go-mode-hook 'feng-go-mode-hook)

(remove-hook 'js2-mode-hook 'moz-minor-mode)

(eval-after-load 'paredit
  '(feng-paredit-key-map))

(eval-after-load 'magit
  '(feng-magic-key-map))

(defun feng-clear-flyspell-keybinding ()
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil))

(eval-after-load 'flyspell
  '(feng-clear-flyspell-keybinding))

(provide 'feng-hooks)
