;;; config-navigation.el --- Navigation and movement configuration -*- lexical-binding: t; -*-

;; Projectile
(use-package projectile
  :config
  (projectile-mode 1))

;; Ace jump mode for quick navigation
(setq ace-jump-mode-case-fold nil)
(use-package ace-jump-mode
  :bind
  (("C-c SPC" . ace-jump-word-mode)
   ("C-s-c SPC" . ace-jump-char-mode)
   ("S-C-u C-c SPC" . ace-jump-line-mode)))

;; Buffer movement
(use-package buffer-move
  :straight (:host github :repo "lukhas/buffer-move" :files ("buffer-move.el"))
  :ensure t)

(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(provide 'config-navigation)
;;; config-navigation.el ends here
