;;; config-terminal.el --- Terminal and shell configuration -*- lexical-binding: t; -*-

;; VTerm configuration
(use-package vterm
  :straight (:host github :repo "akermu/emacs-libvterm")
  :config
  (defun my-vterm-trigger-direnv-reload ()
    "Force direnv to reload in a new vterm"
    (when (locate-dominating-file default-directory ".envrc")
      (vterm-send-string "direnv reload\n")))
  (add-hook 'vterm-mode-hook #'my-vterm-trigger-direnv-reload ())
  
  ;; Custom functions for different vterm modes
  (defun vterm-new-instance ()
    "Create a new vterm instance with unique name."
    (interactive)
    (let ((display-buffer-alist nil)
          (vterm-buffer-name (format "terminal-%d" (random 10000))))
      (vterm)))
  
  ;; Custom popup implementation that doesn't interfere with regular vterm
  (defun my-vterm-popup-toggle ()
    "Toggle a dedicated popup vterm that doesn't interfere with regular vterm buffers."
    (interactive)
    (let ((popup-buffer (get-buffer "*vterm-popup*"))
          (popup-window (get-buffer-window "*vterm-popup*")))
      (if popup-window
          ;; Hide popup - only delete if it's not the sole window
          (when (> (length (window-list)) 1)
            (delete-window popup-window))
        ;; Show popup
        (progn
          (unless popup-buffer
            ;; Create new vterm buffer with specific name, force popup display
            (let ((vterm-buffer-name "*vterm-popup*")
                  (display-buffer-alist 
                   '(("\\*vterm-popup\\*"
                      (display-buffer-at-bottom)
                      (window-height . 0.3)))))
              (setq popup-buffer (vterm))))
          ;; Always display as popup, even if buffer exists
          (let ((window (display-buffer-at-bottom 
                        popup-buffer 
                        '((window-height . 0.3)))))
            (select-window window)))))
  
  ;; Hook to handle vterm process exit in popup
  (defun my-vterm-popup-exit-hook (buffer &optional _)
    "Handle vterm exit in popup - close popup window."
    (when (string= (buffer-name buffer) "*vterm-popup*")
      (let ((popup-window (get-buffer-window "*vterm-popup*")))
        (when (and popup-window (> (length (window-list)) 1))
          (delete-window popup-window)))))
  
  (add-hook 'vterm-exit-functions #'my-vterm-popup-exit-hook)
  
  (defun vterm-popup ()
    "Open vterm as a popup using our custom implementation."
    (interactive)
    (my-vterm-popup-toggle))))



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
