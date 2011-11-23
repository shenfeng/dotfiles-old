(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;;; function copyed from http://paste.lisp.org/display/42657
(defun kill-to-beginning-of-line ()
  "Same as \\[universal-argument] 0 \\[kill-line]."
  (interactive "*")
  (kill-line 0))

(defun ispell-check ()
  "Check region or buffer"
  (interactive)
  (if mark-active
      (if (< (mark) (point))
          (ispell-region (mark) (point))
        (ispell-region (point) (mark)))
    (ispell-buffer)))

(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or regionnn
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
        (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

(defun smart-split ()
  "Split the window into near 80-column sub-windows, try to
equally size every window"
  (interactive)
  (defun compute-width-helper (w)
    "More than 220 column, can be split to 3, else 2"
    (if (> (window-width w) 220) 80
      (+ (/ (window-width w) 2) 1)))
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has
    80 columns."
    (if (> (window-width w) 130)
        (let* ((w2 (split-window w (compute-width-helper w) t))
               (i 0))
          (with-selected-window w2
            (next-buffer)
            (while (and (string-match "^*" (buffer-name)) (< i 20))
              (setq i (1+ i))
              (next-buffer)))
          (smart-split-helper w2))))
  (smart-split-helper nil))

(defun coding-start ()
  (interactive)
  (smart-split)
  (set-face-attribute 'idle-highlight nil
                      :inherit isearch)
  (set-face-attribute 'show-paren-match-face nil
                      :weight 'bold :underline t
                      :foreground "gold1"
                      :overline nil :slant 'normal))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun revert-all-buffers ()
  (interactive)
  (mapc 'revert-buffer (buffer-list)))

(defun feng-elisp-mode-hook ()
  (setq mode-name "el")
  (define-key emacs-lisp-mode-map (kbd "C-M-q") 'session-jump-to-last-change))

(defun feng-paredit-key-map ()
  (define-key paredit-mode-map (kbd "M-<up>") 'beginning-of-defun)
  (define-key paredit-mode-map (kbd "M-<down>") 'end-of-defun)
  ;; include forward
  (define-key paredit-mode-map (kbd "C-M-0") 'paredit-forward-slurp-sexp)
  ;; exclude forward
  (define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
  ;; include backward
  (define-key paredit-mode-map (kbd "C-M-9") 'paredit-backward-slurp-sexp)
  ;; exclude backward
  (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp))

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
  (setq autopair-blink nil)
  (idle-highlight-mode t)
  (yas/minor-mode)
  (define-key js2-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key js2-mode-map (kbd "M-<down>") 'move-text-down)
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
  (setq ac-ignores '("log" "df" "fc" "el" "ei" "if" "ife" "for"))
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
  (setq ac-ignores '("in" "for" "if"))
  (define-key c-mode-map (kbd "C-c C-l") 'copy-line)
  (idle-highlight-mode t)
  (hl-line-mode)
  (autopair-mode)
  (setq autopair-blink nil)
  (yas/minor-mode))

(defun feng-clj-mode-hook ()
  (make-local-variable 'ac-ignores)
  (setq mode-name "clj")
  (let ((ns (clojure-find-package)))    ; defined in clojure-mode.el
    (when (search "-test" ns)
      (save-window-excursion
        (clojure-test-mode t)
        (define-key clojure-test-mode-map [f9] 'clojure-test-run-tests))))
  (define-key clojure-mode-map (kbd "C-M-q") 'session-jump-to-last-change)
  (setq ac-ignores '("ns" "df" "dfp" "dt" "ns" "ss" "resp"))
  (yas/minor-mode))

(defun feng-css-mode-hook ()
  (make-local-variable 'ac-ignores)
  (define-key css-mode-map (kbd "M-<up>") 'move-text-up)
  (define-key css-mode-map (kbd "M-<down>") 'move-text-down)
  (setq ac-ignores '("bg" "bgc" "ff" "fl" "fr" "fs" "fw" "lh" "pa" "pr" "ta"
                     "td" "va" ))
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

;;; http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun feng-select-nth-window (n)
  (let* ((cmp (lambda (l r)
                (if (= (second l) (second r))
                    (< (third l) (third r))
                  (< (second l) (second r)))))
         (windows (sort
                   (mapcar (lambda (w)
                             (cons w (window-edges w)))
                           (window-list)) cmp))
         (index (- (min n (length windows)) 1)))
    (first (nth index windows))))

(defun feng-select-first-window ()
  (interactive)
  (select-window (feng-select-nth-window 1)))

(defun feng-select-second-window ()
  (interactive)
  (select-window (feng-select-nth-window 2)))

(defun feng-select-third-window ()
  (interactive)
  (select-window (feng-select-nth-window 3)))

(defun feng-select-forth-window ()
  (interactive)
  (select-window (feng-select-nth-window 4)))

(defun open-in-desktop ()
  "Open the current file in desktop.
Works in Microsoft Windows, Mac OS X,GNU/Linux ."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore"
                       (replace-regexp-in-string
                        "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin")
    (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (shell-command "gnome-open ."))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg
   lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun no-trancate-line ()
  "Do not trancat lines"
  (setq truncate-lines t))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg
   lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun feng-clear-buffer ()
  (interactive)
  (when (and (not buffer-read-only)
             (string-match "^*" (buffer-name)))
    (delete-region (point-min) (point-max))))

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

(provide 'feng-defuns)
