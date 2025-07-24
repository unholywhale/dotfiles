;;; config-packages.el --- Package management configuration -*- lexical-binding: t; -*-

;; Straight.el package manager setup
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

(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; Essential packages
(use-package diminish)
(use-package general)
(use-package restart-emacs
  :ensure t)

;; 1Password Auth Source
(use-package auth-source-1password
	:config
	(auth-source-1password-enable)
	(setq auth-source-1password-vault "Private")
	(setq auth-source-1password-construct-secret-reference
				(lambda (_backend _type host user _port)
					(cond
					 ((and (string= host "openrouter.ai") (string= user "api"))
						"Private/openrouter.ai/credential")
					 (t
						(mapconcat #'identity (list auth-source-1password-vault host user)
											 "/"))))))

;; Which-key for keybinding hints
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)

  ;; Add descriptive prefixes
  (which-key-add-key-based-replacements
    "C-c d" "debug"
    "C-c e" "environment"
    "C-c l" "lsp"
    "C-c m" "multiple-cursors"
    "C-c n" "notes"
    "C-c r" "reload"
    "C-c t" "terminal"
    "C-c T" "themes"
    "C-c y" "yasnippet"
    "C-x t" "treemacs"
    "M-g" "goto"))

(provide 'config-packages)
;;; config-packages.el ends here
