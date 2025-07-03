;;; config-keybindings.el --- Keybindings configuration -*- lexical-binding: t; -*-

;; Text manipulation
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Newline
(global-set-key (kbd "<C-return>")  'newline)

;; Vim-like word movement and deletion
(global-set-key (kbd "M-f") 'my/forward-word-or-whitespace)
(global-set-key (kbd "M-b") 'my/backward-word-or-whitespace)
(global-set-key (kbd "M-d") 'my/kill-word-or-whitespace)
(global-set-key (kbd "M-<backspace>") 'my/backward-kill-word-or-whitespace)

;; Reload init file function
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (set-appearance))

(provide 'config-keybindings)
;;; config-keybindings.el ends here