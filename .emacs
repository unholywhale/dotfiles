(setq dotfiles-dir "~/dotfiles")
(load-file (format "%s/%s" dotfiles-dir "functions.el"))
(setq user-init-file load-file-name)

(if (eq system-type 'darwin)
    (progn
      (setq gcc-lib-path "/opt/homebrew/lib/gcc/current/:/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13/")
      (setenv "LIBRARY_PATH"
	      (if (getenv "LIBRARY_PATH")
		  (format "%s:%s" gcc-lib-path (getenv "LIBRARY_PATH"))
		gcc-lib-path))
      (setenv "LD_LIBRARY_PATH"
	      (if (getenv "LD_LIBRARY_PATH")
		  (format "%s:%s" gcc-lib-path (getenv "LD_LIBRARY_PATH"))
		gcc-lib-path))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
	 '("4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0" "95167736741bef2ad3e0543ed545dada5b95fef309883253387a2b14ab67db8d" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e" "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74" "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "dbf0cd368e568e6139bb862c574c4ad4eec1859ce62bc755d2ef98f941062441" "f079ef5189f9738cf5a2b4507bcaf83138ad22d9c9e32a537d61c9aae25502ef" "755fc94932731e7c043d6374bcf488a00cc84235d4a3ca0b412d061281be2c64" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

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

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
	(set-appearance))

;; Themes
(use-package zenburn-theme
  :ensure t)
;; (use-package nord-theme
;; 	:ensure t)
;; (use-package timu-macos-theme
;;   :ensure t)
;; (use-package material-theme
;;   :ensure t)
;; (use-package modus-themes
;; 	:ensure t)


(setq-default tab-width 2)
(setq confirm-kill-processes nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(delete-selection-mode 1)

;; Transparency
;;(add-to-list 'default-frame-alist '(alpha-background . 95))

(defun frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.4) (left . 0.4)))))

(defun set-appearance ()
	(message "Setting appearance...")
	(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :foundry "JB" :slant 'normal :weight 'regular :height 120 :width 'normal)
	;; (set-face-attribute 'yascroll:thumb-text-area nil :background "dark gray")
	;; (set-face-attribute 'yascroll:thumb-fringe nil :background "dark gray" :foreground "dark gray")
	(load-theme 'modus-vivendi-tinted)
	(when window-system
		(set-frame-size (selected-frame) 160 40)
		;;(frame-recenter)
		))
(if (daemonp)
		(add-hook 'after-make-frame-functions
							(lambda (frame)
								(with-selected-frame frame
									(set-appearance))))
	(set-appearance))

(use-package diminish)

;; ;; Music
;; (use-package emms
;; 	:ensure t
;; 	:config
;; 	(require 'emms-setup)
;; 	(require 'emms-player-mpd)
;; 	(setq emms-seek-seconds 5)
;; 	(setq emms-player-list '(emms-player-mpd))
;; 	(setq emms-info-functions '(emms-info-mpd))
;; 	(setq emms-player-mpd-server-name "localhost")
;; 	(setq emms-player-mpd-server-port "6600"))

;; Dashboard
(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner nil) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner (concat user-emacs-directory "images/dtmacs-logo.png"))  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  ;; (dashboard-modify-heading-icons '((recents . "file-text")
  ;;                             (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package multiple-cursors
  :bind (("C-<" . 'mc/mark-previous-like-this)
	 ("C->" . 'mc/mark-next-like-this)
	 ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
	 (:prefix "C-c m" :prefix-map mc-map
		 ("d" . 'mc/mark-all-like-this-dwim)
		 ("a" . 'mc/mark-all-like-this)
		 ("n" . 'mc/insert-numbers)
		 ("l" . 'mc/insert-letters))))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package direnv
	:diminish
	:ensure t
  :config
	(setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package lsp-mode
	:diminish
  :commands lsp
  ;;:hook ((c++-mode python-ts-mode) . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package dap-mode)
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package yaml-mode)
(use-package qml-mode)

(use-package tree-sitter
  :hook
  ((css-mode 
    c-or-c++-mode
    c-mode
    c++-mode
    cmake-mode
    js-mode
    json-mode
    python-mode
;;    sh-mode
    typescript-mode
    yaml-mode)
   . tree-sitter-enable)
  (tree-sitter-after-on . lsp-deferred)
  :preface
  (defun tree-sitter-enable ()
    (tree-sitter-mode t))
  :defer t)

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

;; which-key
(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.3
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))


;; Org Table of Contents
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))


;; Move buffers
(use-package buffer-move
  :straight (:host github :repo "lukhas/buffer-move" :files ("buffer-move.el"))
  :ensure t)
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

(use-package general)


(use-package nerd-icons
  :custom
  (nerd-icons-font-family "JetBrainsMono Nerd Font"))

;; Dired
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))
;; (define-key dired-mode-map (kbd "C-f") 'dired-find-file)
;; (define-key dired-mode-map (kbd "C-b") 'dired-up-directory)
;; (use-package nerd-icons-dired
;;   :hook
;;   (dired-mode . nerd-icons-dired-mode))
(if (eq system-type 'darwin)
    (setq insert-directory-program "gls" dired-use-ls-dired t))
(setq dired-listing-switches "-agh --group-directories-first")

;; Terminal
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
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  ;; (eshell-toggle-use-projectile-root t)
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

;; Projectile
(use-package projectile
  :config
  (projectile-mode 1))

;; Beacon
(use-package beacon
  :straight (:host github :repo "Malabarba/beacon" :files("beacon.el"))
  :ensure t
  :init
  (beacon-mode 1)
  :config
  (setq beacon-blink-when-window-scrolls nil))

;; Scroll bar
(scroll-bar-mode 0)
;; (use-package yascroll
;;   :straight (:host github :repo "emacsorphanage/yascroll" :files ("yascroll.el"))
;;   :ensure t
;;   :init
;;   (global-yascroll-bar-mode))

;; Completion
(use-package orderless)

(use-package vertico
  :init
  (vertico-mode)
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions")
  (setq completion-styles '(substring orderless basic partial-completion))
  (require 'vertico-directory)
  ;; Enable vertico-directory features here
  )

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
	 ("C-s"   . consult-line)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;;("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
	 ;;:map goto-map
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
	 :map search-map
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s"   . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("C-s"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                 ;; Custom M-# bindings for fast register access

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package marginalia
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; ;; Pair parentheses, brackets etc.
;; (electric-pair-mode)
(use-package smartparens
	:init
	(smartparens-global-mode))

(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-word-mode)
  ("C-s-c SPC" . ace-jump-char-mode)
  ("S-C-u C-c SPC" . ace-jump-line-mode))

(use-package company
  :diminish
	:bind (("C-c y" . company-yasnippet))
  :init
  (global-company-mode)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.1)
	(company-require-match nil)
  (company-echo-delay 0.1)
  (company-dabbrev-downcase 0)
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 3))

;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode))

(use-package cape)

(use-package yasnippet
	:config
	(add-to-list 'yas-snippet-dirs (format "%s/%s/%s" dotfiles-dir "emacs.stuff" "yasnippets"))
	(yas-global-mode 1))
(use-package yasnippet-snippets)

;; Org
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-startup-with-visual-line-mode t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Git
(use-package magit)

(setq vc-follow-symlinks t)

;; Syntax highlighting
;; (use-package cmake-mode)
(use-package python-mode)
(use-package dockerfile-mode)
(use-package yuck-mode
  :mode "\\.yuck\\'")
(use-package markdown-mode)
(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))
(use-package rainbow-delimiters
  :diminish
  :hook ((org-mode prog-mode) . rainbow-delimiters-mode))
(use-package nix-mode
  :mode "\\.nix\\'")


;; Virtualenv
(setenv "WORKON_HOME" "~/personal/envs/")
;(pyvenv-workon "py3.12")
;;(python-mode . ((pyvenv-activate . "~/envs/py3.12_arm")))

(use-package pyvenv
    :ensure t)

;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(setq copilot-indent-offset-warning-disable t)
;; Accept copilot completion on shift-tab
(define-key copilot-mode-map (kbd "<backtab>") 'copilot-accept-completion-by-line)

;; Keybindings
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-return>")  'newline)

;; (use-package telega
;; 	:init
;; 	(setq telega-use-images t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
