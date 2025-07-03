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

;; Reload init file function
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (set-appearance))

(provide 'config-keybindings)
;;; config-keybindings.el ends here