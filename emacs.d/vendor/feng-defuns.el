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

(defun feng-revert-buffer ()
  (interactive)
  (let ((p (point)))
    (revert-buffer)
    (goto-char p)))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun revert-all-buffers ()
  (interactive)
  (mapc 'revert-buffer (buffer-list)))

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

;; Originally from stevey, adapted to support moving to a new
;; directory.
;; http://stackoverflow.com/questions/384284/can-i-rename-an-open-file-in-emacs
(defun feng-rename (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

(provide 'feng-defuns)
