;;; config-ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;; Basic UI settings
(setq-default tab-width 2)
(setq confirm-kill-processes nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)
(scroll-bar-mode 0)

;; Frame settings
(add-to-list 'default-frame-alist
             '(internal-border-width . 8))

;; Transparency (commented out)
;;(add-to-list 'default-frame-alist '(alpha-background . 95))

;; Themes
(use-package zenburn-theme
  :ensure t)
(use-package timu-spacegrey-theme
  :ensure t)
;; Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font"))

;; Dashboard
(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5)
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook))

;; Visual enhancements
(use-package beacon
  :straight (:host github :repo "Malabarba/beacon" :files("beacon.el"))
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-blink-when-window-scrolls nil))

(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :diminish
  :hook ((org-mode prog-mode) . rainbow-delimiters-mode))

;; Line numbers for programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Frame centering function
(defun frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.4) (left . 0.4)))))

;; Appearance setup function
(defun set-appearance ()
  (message "Setting appearance...")
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :foundry "JB" :slant 'normal :weight 'regular :height 120 :width 'normal)
  ;; Load theme (needed for both daemon and non-daemon to ensure it's applied to frames)
  (when (and (not noninteractive) (locate-library "timu-spacegrey-theme"))
    (load-theme 'timu-spacegrey t))
  (when window-system
    (set-frame-size (selected-frame) 160 40)))

;; Apply appearance settings
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-appearance))))
  (set-appearance))

;; Dired configuration
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-agh --group-directories-first")

(provide 'config-ui)
;;; config-ui.el ends here
