;;; config-terminal.el --- Terminal and shell configuration -*- lexical-binding: t; -*-

;; VTerm configuration
(use-package vterm
  :straight (:host github :repo "akermu/emacs-libvterm")
  :config
  (defun my-vterm-trigger-direnv-reload ()
    "Force direnv to reload in a new vterm"
    (when (locate-dominating-file default-directory ".envrc")
      (vterm-send-string "direnv reload\n")))
  (add-hook 'vterm-mode-hook #'my-vterm-trigger-direnv-reload ()))

(use-package vterm-toggle
  :after vterm
  :bind (("C-`" . vterm-toggle))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  (reusable-frames . visible)
                  (window-height . 0.3))))

;; Eshell configuration
(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(provide 'config-terminal)
;;; config-terminal.el ends here