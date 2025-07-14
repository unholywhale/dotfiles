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

;; Backup file settings (prevents delays when opening files with ~ backups)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; Auto-save settings to prevent recovery prompts
(setq auto-save-default t)
(setq auto-save-timeout 20)
(setq auto-save-interval 200)
;; Suppress auto-save recovery prompts
(setq auto-save-list-file-prefix nil)

;; Disable auto-save recovery checking entirely
(setq auto-save-visited-mode nil)
;; Disable the auto-save recovery prompt
(advice-add 'after-find-file :around
            (lambda (orig-fun &rest args)
              "Skip auto-save recovery prompts by setting noauto to t."
              (apply orig-fun (append (butlast args 3) '(nil nil t)))))

;; Frame settings
(add-to-list 'default-frame-alist
             '(internal-border-width . 5))

;; Transparency (commented out)
;;(add-to-list 'default-frame-alist '(alpha-background . 95))

;; Theme management
(defvar my/current-theme 'modus-operandi-tinted
  "Current theme to use.")

(defvar my/available-themes '(timu-spacegrey
															zenburn
															modus-operandi
															modus-operandi-tinted
															modus-vivendi
															modus-vivendi-tinted
															material
															adwaita
															dichromacy
															leuven-dark
															leuven
															manoj-dark
															misterioso
															tango-dark
															tango
															tsdh-dark
															tsdh-light
															wheatgrass
															whiteboard
															wombat)
  "List of available themes.")

;; Install theme packages
(use-package zenburn-theme
  :ensure t)
(use-package timu-spacegrey-theme
  :ensure t)
;; Icons
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font"))

;; Use scratch buffer instead of dashboard
(setq initial-buffer-choice nil)
(setq initial-scratch-message nil)

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

;; Mood-line - minimal modeline
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode)
  ;; Add padding to modeline without changing font size
  (custom-set-faces
   '(mode-line ((t (:box (:line-width 6 :style flat-button)))))
   '(mode-line-inactive ((t (:box (:line-width 6 :style flat-button)))))))

;; Theme loading function
(defun my/load-theme (theme)
  "Load the specified theme safely."
  (when (and (not noninteractive) theme)
    (let ((theme-package (format "%s-theme" theme)))
      (when (locate-library theme-package)
        ;; Disable all custom themes first
        (mapc #'disable-theme custom-enabled-themes)
        ;; Load the new theme
        (load-theme theme t)
        (setq my/current-theme theme)
        (message "Loaded theme: %s" theme)))))

;; Appearance setup function
(defun set-appearance ()
  (message "Setting appearance...")
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :foundry "JB" :slant 'normal :weight 'regular :height 120 :width 'normal)
  ;; Load current theme
  (my/load-theme my/current-theme)
  (when window-system
    (set-frame-size (selected-frame) 120 35)))

;; Apply appearance settings
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-appearance))))
  (set-appearance))

(provide 'config-ui)
;;; config-ui.el ends here
