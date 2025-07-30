;;; config-development.el --- Development tools configuration -*- lexical-binding: t; -*-

;; Performance optimization
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Prevent problematic LSP modules from loading
(with-eval-after-load 'lsp-mode
  (require 'lsp-mode)
  ;; Remove roslyn from the client packages list entirely
  (setq lsp-client-packages (delq 'lsp-roslyn lsp-client-packages)))

(setq vc-follow-symlinks t)

;; Environment management
(use-package direnv
  :diminish
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (setq direnv-use-faces-in-summary t)
  ;; Disable automatic direnv switching to avoid LSP conflicts
  ;; Use manual C-c e r or C-c e R instead
  (direnv-mode))

;; LSP configuration
(use-package lsp-mode
  :diminish
  :commands lsp lsp-deferred
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-diagnostics-provider :flycheck)
  ;; Use corfu for completion
  (setq lsp-completion-provider :capf)
  ;; Performance: limit workspace scanning
  (setq lsp-enable-file-watchers nil)
  (setq lsp-auto-guess-root t)  ; Let it find project root properly
  ;; Reduce message verbosity
  (setq lsp-log-io nil)
  (setq lsp-print-performance nil)
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setq lsp-eldoc-render-all nil)
	(setq lsp-signature-auto-activate nil)
	(setq lsp-inlay-hint-enable nil)
  ;; Project isolation settings
  (setq lsp-auto-guess-root t)
  (setq lsp-restart 'auto-restart)
  (setq lsp-keep-workspace-alive nil)
  ;; Force separate workspaces per project
  (setq lsp-enable-file-watchers nil)
  (setq lsp-server-install-dir (expand-file-name "lsp/" user-emacs-directory))
  ;; Disable headerline breadcrumb to prevent flickering during restarts
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable t)
  :config
  ;; Disable problematic clients
  (setq lsp-disabled-clients '(roslyn mspyls pylsp pyls ruff-lsp semgrep-ls))
  ;; Suppress verbose initialization and connection messages
  (defun my/lsp-message-filter (orig-fun &rest args)
    "Filter out verbose LSP initialization and connection messages."
    (let ((message (car args)))
      (unless (and (stringp message)
                   (or (string-match-p "initialized successfully in folders:" message)
                       (string-match-p "Connected to \\[pyright:" message)
                       (string-match-p "LSP :: Connected to" message)))
        (apply orig-fun args))))
  (advice-add 'lsp--info :around #'my/lsp-message-filter)
  (advice-add 'lsp-workspace-show-message :around #'my/lsp-message-filter))

(use-package lsp-ui
	:ensure
	:commands lsp-ui-mode
	:config
	(setq lsp-ui-peek-always-show t)
	(setq lsp-ui-sideline-show-hover t)
	(setq lsp-ui-doc-enable nil))

;; Debug Adapter Protocol (DAP) for interactive debugging
(use-package dap-mode
  :ensure t
  :hook ((dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
         (dap-session-created . (lambda (arg) (dap-ui-mode 1))))
  :config
  ;; Enable dap-mode features
  (dap-auto-configure-mode)

  ;; UI improvements
  (setq dap-auto-configure-features '(sessions locals controls tooltip))

  ;; Python debugging configuration
  (require 'dap-python)

  ;; Configure Python debugger
  (setq dap-python-debugger 'debugpy)

  ;; Load hydra (part of dap-mode)
  (require 'dap-hydra)

  ;; Load UI (part of dap-mode)
  (require 'dap-ui))

(use-package lsp-pyright
  :ensure t
  :hook ((python-mode python-ts-mode) . lsp-deferred)
  :config
  ;; Limit Pyright to current project only
  (setq lsp-pyright-multi-root nil)
  ;; Don't auto-search for additional Python paths
  (setq lsp-pyright-auto-search-paths nil)
  ;; Track LSP project changes
  (add-hook 'lsp-after-open-hook
            (lambda ()
              (when (derived-mode-p 'python-mode 'python-ts-mode)
                (setq my/last-lsp-project 
                      (or (locate-dominating-file default-directory ".envrc")
                          (when (fboundp 'projectile-project-root)
                            (projectile-project-root))
                          default-directory))))))

(use-package flycheck
	:ensure t
	:init (global-flycheck-mode))

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

;; Rust LSP configuration
;; (use-package rust-mode
;;   :ensure t
;;   :hook (rust-mode . lsp-deferred))

;; Enhanced Rust support with additional features
(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; Use rust-analyzer as the LSP server
  (setq rustic-analyzer-command '("rust-analyzer"))
  (setq rustic-format-trigger 'on-compile)
  (setq rustic-lsp-client 'lsp-mode)
	(setq lsp-rust-analyzer-cargo-extra-env #s(hash-table size 1 test equal data ()))
	;; LSP hints
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints nil)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq lsp-rust-analyzer-display-closure-return-type-hints nil)
  (setq lsp-rust-analyzer-display-parameter-hints nil)
   ;; Configure rustfmt to use edition 2024
  (setq rustic-rustfmt-args "--edition 2024"))

;; Built-in tree-sitter (Emacs 29+)
(when (treesit-available-p)
  ;; Configure language sources
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
					(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
					(rust "https://github.com/tree-sitter/tree-sitter-rust")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  
  ;; Auto-enable treesit modes for supported languages
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (c-mode . c-ts-mode)
					(go-mode . go-ts-mode)
					(rust-mode . rust-ts-mode)
          (c++-mode . c++-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  
  ;; Install grammars automatically if missing
  (defun my/install-treesit-grammars ()
    "Install tree-sitter grammars for configured languages."
    (interactive)
    (dolist (lang '(python css javascript json c cpp rust typescript yaml bash))
      (unless (treesit-language-available-p lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error (message "Failed to install grammar for %s: %s" lang err))))))
  
  ;; Install grammars on first load (but don't block startup)
  (run-with-idle-timer 1 nil #'my/install-treesit-grammars))

;; Programming language modes
(use-package yaml-mode)
(use-package qml-mode)
(use-package python-mode)
(use-package rust-mode)
(use-package dockerfile-mode)
(use-package yuck-mode
  :mode "\\.yuck\\'")
(use-package markdown-mode)
(use-package nix-mode
  :mode "\\.nix\\'")

;; Python virtual environment
(setenv "WORKON_HOME" "~/personal/envs/")
(use-package pyvenv
    :ensure t)

;; Parentheses handling
(use-package smartparens
	:init
	(smartparens-global-mode)
	:config
	(sp-local-pair 'emacs-lisp-mode "'" "'" :actions '(wrap autoskip navigate escape))
	(sp-local-pair 'emacs-lisp-mode "`" "`" :actions '(wrap autoskip navigate escape))
	(setq sp-autodelete-pair nil))

(provide 'config-development)
;;; config-development.el ends here


