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

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . 'mc/mark-previous-like-this)
         ("C->" . 'mc/mark-next-like-this)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
         (:prefix "C-c m" :prefix-map mc-map
                  ("d" . 'mc/mark-all-like-this-dwim)
                  ("a" . 'mc/mark-all-like-this)
                  ("n" . 'mc/insert-numbers)
                  ("l" . 'mc/insert-letters))))

;; Dired configuration
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-agh --group-directories-first")

(provide 'config-navigation)
;;; config-navigation.el ends here
