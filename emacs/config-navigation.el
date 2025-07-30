;;; config-navigation.el --- Navigation and movement configuration -*- lexical-binding: t; -*-

;; Projectile
(use-package projectile
  :config
  (projectile-mode 1))

;; Ace jump mode for quick navigation
(setq ace-jump-mode-case-fold nil)
(use-package ace-jump-mode)

;; Buffer movement
(use-package buffer-move
  :straight (:host github :repo "lukhas/buffer-move" :files ("buffer-move.el"))
  :ensure t)

(use-package yafolding
	:ensure t)

;; Multiple cursors
(use-package multiple-cursors)

;; Treemacs - file tree sidebar
(use-package treemacs
  :ensure t
  :config
  ;; Basic treemacs settings
  (setq treemacs-width 30)
  (setq treemacs-follow-mode t)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-fringe-indicator-mode 'always)
  ;; Monochrome icons - folders have icons, files don't
  (setq treemacs-no-png-images t)
  (setq treemacs-show-hidden-files t)
  ;; Remove file icons - just show file names
  (setq treemacs-icon-fallback ""))

;; Treemacs projectile integration
(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; Dired configuration
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-agh --group-directories-first")

(provide 'config-navigation)
;;; config-navigation.el ends here
