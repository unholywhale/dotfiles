;;; functions.el --- Custom utility functions -*- lexical-binding: t; -*-

;; Direnv utilities
(defun my/direnv-refresh ()
  "Manually refresh direnv environment for current buffer."
  (interactive)
  (when (buffer-file-name)
    (let* ((file-dir (file-name-directory (buffer-file-name)))
           (envrc-dir (locate-dominating-file file-dir ".envrc")))
      (if envrc-dir
          (let ((default-directory envrc-dir))
            (direnv-update-environment)
            ;; Update the project tracking variable
            (setq my/last-direnv-project envrc-dir)
            (message "Direnv environment refreshed for %s" (abbreviate-file-name envrc-dir)))
        (message "No .envrc found for %s" (abbreviate-file-name file-dir))))))

(defun my/direnv-refresh-with-lsp ()
  "Manually refresh direnv environment and restart LSP."
  (interactive)
  (my/direnv-refresh)
  (when (and (featurep 'lsp-mode)
             (derived-mode-p 'python-mode 'python-ts-mode))
    (let* ((file-dir (file-name-directory (buffer-file-name)))
           (project-root (or (locate-dominating-file file-dir ".envrc")
                            (when (fboundp 'projectile-project-root)
                              (projectile-project-root))
                            file-dir)))
      ;; Simple approach: just restart workspace
      (lsp-restart-workspace)
      (message "LSP restarted for project: %s" (abbreviate-file-name project-root)))))

(defun my/show-direnv-status ()
  "Show current direnv status and environment variables."
  (interactive)
  (if (bound-and-true-p direnv-mode)
      (let* ((file-dir (if (buffer-file-name)
                          (file-name-directory (buffer-file-name))
                        default-directory))
             (envrc-dir (locate-dominating-file file-dir ".envrc"))
             (python-path (getenv "PYTHONPATH"))
             (virtual-env (getenv "VIRTUAL_ENV"))
             (lsp-root (when (and (featurep 'lsp-mode) (lsp-workspaces))
                        (condition-case nil
                            (lsp-workspace-root (cl-first (lsp-workspaces)))
                          (error "error")))))
        (if envrc-dir
            (message "Direnv: %s | VirtualEnv: %s | LSP: %s"
                     (abbreviate-file-name envrc-dir)
                     (or (when virtual-env (file-name-nondirectory virtual-env)) "none")
                     (or (when lsp-root (abbreviate-file-name lsp-root)) "none"))
          (message "No .envrc found for %s" (abbreviate-file-name file-dir))))
    (message "Direnv mode not enabled")))

(defun my/force-lsp-restart ()
  "Force restart LSP workspace for current buffer."
  (interactive)
  (when (and (featurep 'lsp-mode) (lsp-workspaces))
    (lsp-restart-workspace)
    (message "LSP workspace restarted")))





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

(defun my/move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun my/move-text-up (arg)
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

;; Notes directory configuration
(defvar my/notes-directory "~/personal/notes"
  "Directory containing personal notes.")

(defun my/open-notes-directory ()
  "Open the notes directory in dired."
  (interactive)
  (dired (expand-file-name my/notes-directory)))

;; Reload init file function
(defun reload-init-file ()
  "Reload the init file and apply appearance settings."
  (interactive)
  (load-file user-init-file)
  (set-appearance))

(provide 'functions)
;;; functions.el ends here
