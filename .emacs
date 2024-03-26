(load-file "~/.emacs.personal/functions.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-blink-when-window-scrolls nil)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("755fc94932731e7c043d6374bcf488a00cc84235d4a3ca0b412d061281be2c64" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default))
 '(global-yascroll-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight regular :height 130 :width normal))))
 '(yascroll:thumb-fringe ((t (:background "dark gray" :foreground "dark gray"))))
 '(yascroll:thumb-text-area ((t (:background "dark gray")))))

;; Straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Packages
(use-package zenburn-theme
  :ensure t)

(use-package vterm
  :straight (:host github :repo "akermu/emacs-libvterm"))



(use-package vertico
  :init
  (vertico-mode)
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions")
  (setq completion-styles '(basic substring partial-completion orderless))
  (require 'vertico-directory)
  ;; Enable vertico-directory features here
  )

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless)

(find-file "~/.emacs")

;; (use-package ivy
;;   :diminish
;; ;;  :bind (("C-x b" . ivy-switch-buffer))
;;   :config
;;   (ivy-mode 1))

(use-package counsel
  :bind (
	 ;;("C-M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package company
  :init
  (global-company-mode))

(use-package pyvenv
  :ensure t)

(setenv "WORKON_HOME" "~/envs/")
;;(pyvenv-workon "py3.12_arm")
;;(python-mode . ((pyvenv-activate . "~/envs/py3.12_arm")))

;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (when window-system
;;   (set-frame-position (selected-frame) 250 170)
;;   (set-frame-size (selected-frame) 135 40))

(defun frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))
(frame-recenter)

(modify-frame-parameters nil '((width . 135) (height . 40)))

(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(setq ring-bell-function 'ignore)

(use-package timu-macos-theme
  :ensure t)


;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(setq copilot-indent-offset-warning-disable t)
;; Accept copilot completion on shift-tab
(define-key copilot-mode-map (kbd "<backtab>") 'copilot-accept-completion-by-line)


(define-key global-map (kbd "M-<up>") 'move-text-up)
(define-key global-map (kbd "M-<down>") 'move-text-down)

(use-package sublimity
  :straight (:host github :repo "zk-phi/sublimity" :files ("*.el"))
  :ensure t
  :init
  (sublimity-mode)
  :config
  (require 'sublimity-attractive))


(use-package beacon
  :straight (:host github :repo "Malabarba/beacon" :files("beacon.el"))
  :ensure t
  :init
  (beacon-mode 1))

(scroll-bar-mode 0)
(use-package yascroll
  :straight (:host github :repo "emacsorphanage/yascroll" :files ("yascroll.el"))
  :ensure t)

(use-package buffer-move
  :straight (:host github :repo "lukhas/buffer-move" :files ("buffer-move.el"))
  :ensure t)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(global-set-key (kbd "<C-return>")  'newline)


;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (tab-bar-mode 0)
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-word-mode)
  ("C-s-c SPC" . ace-jump-char-mode)
  ("S-C-u C-c SPC" . ace-jump-line-mode))

;; (use-package emacs-mini-frame
;;   :straight (:host github :repo "muffinmad/emacs-mini-frame" :files ("mini-frame.el"))
;;   :ensure t)

(use-package markdown-mode)
