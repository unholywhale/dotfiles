;;; config-development.el --- Development tools configuration -*- lexical-binding: t; -*-

;; Performance optimization
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Environment management
(use-package direnv
  :diminish
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

;; LSP configuration
(use-package lsp-mode
  :diminish
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package dap-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Tree-sitter configuration
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

;; Programming language modes
(use-package yaml-mode)
(use-package qml-mode)
(use-package python-mode)
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

;; Copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(setq copilot-indent-offset-warning-disable t)
;; Accept copilot completion on shift-tab
(define-key copilot-mode-map (kbd "<backtab>") 'copilot-accept-completion-by-line)

;; Parentheses handling
(use-package smartparens
  :init
  (smartparens-global-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-<" . 'mc/mark-previous-like-this)
         ("C->" . 'mc/mark-next-like-this)
         ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)
         (:prefix "C-c m" :prefix-map mc-map
                  ("d" . 'mc/mark-all-like-this-dwim)
                  ("a" . 'mc/mark-all-like-this)
                  ("n" . 'mc/insert-numbers)
                  ("l" . 'mc/insert-letters))))

(provide 'config-development)
;;; config-development.el ends here