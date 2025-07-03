;;; functions.el --- Custom utility functions -*- lexical-binding: t; -*-

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
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

;; Vim-like word movement and deletion functions
(defun my/forward-word-or-whitespace ()
  "Move forward to the end of the next word or whitespace sequence."
  (interactive)
  (if (looking-at "\\s-\\{2,\\}")
      ;; If at 2+ whitespaces, skip all whitespace
      (skip-chars-forward " \t\n")
    ;; Otherwise, move to end of current word (including single spaces)
    (forward-word)))

(defun my/backward-word-or-whitespace ()
  "Move backward to the beginning of the previous word or whitespace sequence."
  (interactive)
  (if (looking-back "\\s-\\{2,\\}" (line-beginning-position))
      ;; If preceded by 2+ whitespaces, skip all whitespace backward
      (skip-chars-backward " \t\n")
    ;; Otherwise, move to beginning of current/previous word
    (backward-word)))

(defun my/kill-word-or-whitespace ()
  "Kill forward word or whitespace sequence (Vim-like dw behavior)."
  (interactive)
  (let ((start (point)))
    (cond
     ;; If at 2+ whitespaces, kill all whitespace
     ((looking-at "\\s-\\{2,\\}")
      (skip-chars-forward " \t\n"))
     ;; If at a quote, kill the entire quoted string
     ((looking-at "[\"']")
      (let ((quote-char (char-after)))
        (forward-char) ; move past opening quote
        (while (and (not (eobp))
                    (not (= (char-after) quote-char)))
          (if (= (char-after) ?\\)
              (forward-char 2) ; skip escaped character
            (forward-char)))
        (when (= (char-after) quote-char)
          (forward-char)))) ; include closing quote
     ;; Otherwise, kill to end of word (including single spaces)
     (t
      (forward-word)))
    (kill-region start (point))))

(defun my/backward-kill-word-or-whitespace ()
  "Kill backward word or whitespace sequence."
  (interactive)
  (let ((end (point)))
    (cond
     ;; If preceded by 2+ whitespaces, kill all whitespace backward
     ((looking-back "\\s-\\{2,\\}" (line-beginning-position))
      (skip-chars-backward " \t\n"))
     ;; If at a quote (looking back), kill the entire quoted string
     ((looking-back "[\"']" (line-beginning-position))
      (let ((quote-char (char-before)))
        (backward-char) ; move past closing quote
        (while (and (not (bobp))
                    (not (and (= (char-before) quote-char)
                              ;; Check if this quote is not escaped
                              (or (= (point) (line-beginning-position))
                                  (not (= (char-before (1- (point))) ?\\))))))
          (backward-char))
        (when (and (not (bobp)) (= (char-before) quote-char))
          (backward-char)))) ; include opening quote
     ;; Otherwise, kill to beginning of word
     (t
      (backward-word)))
    (kill-region (point) end)))

(provide 'functions)
;;; functions.el ends here)
