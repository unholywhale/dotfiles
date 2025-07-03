;;; config-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;; Org mode configuration
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-with-visual-line-mode t)

;; Org Table of Contents
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Git integration
(use-package magit)
(setq vc-follow-symlinks t)

(provide 'config-org)
;;; config-org.el ends here