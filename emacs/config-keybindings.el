;;; config-keybindings.el --- Keybindings configuration -*- lexical-binding: t; -*-
;; Window movement
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x o") 'ace-select-window)

;; Text manipulation
(global-set-key (kbd "M-<up>") 'my/move-text-up)
(global-set-key (kbd "M-<down>") 'my/move-text-down)

;; Text scaling
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Newline
(global-set-key (kbd "<C-return>")  'newline)

;; Vim-like word movement and deletion
(global-set-key (kbd "M-f") 'my/forward-word-or-whitespace)
(global-set-key (kbd "M-b") 'my/backward-word-or-whitespace)
(global-set-key (kbd "M-d") 'my/kill-word-or-whitespace)
(global-set-key (kbd "M-<backspace>") 'my/backward-kill-word-or-whitespace)
(global-set-key (kbd "C-<backspace>") 'my/backward-kill-word-or-whitespace)

;; Direnv utilities
(global-set-key (kbd "C-c e r") 'my/direnv-refresh)
(global-set-key (kbd "C-c e R") 'my/direnv-refresh-with-lsp)
(global-set-key (kbd "C-c e s") 'my/show-direnv-status)
(global-set-key (kbd "C-c l r") 'my/force-lsp-restart)

;; Terminal utilities
(global-set-key (kbd "C-`") 'my-vterm-popup-toggle)
(global-set-key (kbd "C-c t n") 'vterm-new-instance)
(global-set-key (kbd "C-c t p") 'vterm-popup)

;; Navigation utilities
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)
(global-set-key (kbd "C-s-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "S-C-u C-c SPC") 'ace-jump-line-mode)

;; Multiple cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c m d") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m n") 'mc/insert-numbers)
(global-set-key (kbd "C-c m l") 'mc/insert-letters)

;; Treemacs
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t s") 'treemacs-select-window)
(global-set-key (kbd "C-1") 'treemacs)

;; Consult bindings
(global-set-key (kbd "C-c M-x") 'consult-mode-command)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-c p s") 'consult-ripgrep)
(global-set-key (kbd "C-c p f") 'consult-find)
(global-set-key (kbd "C-c i") 'consult-info)
(global-set-key (kbd "C-x M-:") 'consult-complex-command)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)
(global-set-key (kbd "C-x t b") 'consult-buffer-other-tab)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)
(global-set-key (kbd "M-#") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store)
(global-set-key (kbd "C-M-#") 'consult-register)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "M-g e") 'consult-compile-error)
(global-set-key (kbd "M-g f") 'consult-flymake)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-g M-g") 'consult-goto-line)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g k") 'consult-global-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)

;; Completion
(global-set-key (kbd "C-c y") 'cape-yasnippet)

;; Embark
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-.") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

;; Marginalia
(global-set-key (kbd "M-A") 'marginalia-cycle)

;; Buffer movement keybindings
(global-set-key (kbd "<C-s-up>")     'buf-move-up)
(global-set-key (kbd "<C-s-down>")   'buf-move-down)
(global-set-key (kbd "<C-s-left>")   'buf-move-left)
(global-set-key (kbd "<C-s-right>")  'buf-move-right)

;; Visual selection mode keybindings
(global-set-key (kbd "C-SPC") 'my/enhanced-set-mark-command)
(global-set-key (kbd "C-@") 'set-mark-command)
(global-set-key (kbd "C-c d t") 'my/toggle-debug)
(global-set-key (kbd "C-c d i") 'my/show-buffer-info)

;; Notes directory
(global-set-key (kbd "C-c n d") 'my/open-notes-directory)

;; Theme switching (moved to C-c T to avoid conflict with terminal)
(global-set-key (kbd "C-c T s") 'my/switch-theme)
(global-set-key (kbd "C-c T c") 'my/cycle-theme)
(global-set-key (kbd "C-c T t") 'my/set-theme)

;; Configuration reloading
(global-set-key (kbd "C-c r r") 'my/reload-config-or-restart)
(global-set-key (kbd "C-c r c") 'my/reload-config-clean)
(global-set-key (kbd "C-c r s") 'reload-init-file)
(global-set-key (kbd "C-c r d") 'my/restart-emacs-daemon)

;; Python debugging with dap-mode
(global-set-key (kbd "C-c d b") 'dap-breakpoint-toggle)
(global-set-key (kbd "C-c d d") 'dap-debug)
(global-set-key (kbd "C-c d f") 'my/python-debug-current-file)
(global-set-key (kbd "C-c d F") 'my/python-debug-current-file-alt)
(global-set-key (kbd "C-c d r") 'dap-debug-restart)
(global-set-key (kbd "C-c d s") 'dap-step-in)
(global-set-key (kbd "C-c d n") 'dap-next)
(global-set-key (kbd "C-c d o") 'dap-step-out)
(global-set-key (kbd "C-c d c") 'dap-continue)
(global-set-key (kbd "C-c d q") 'dap-disconnect)
(global-set-key (kbd "C-c d u") 'dap-ui-mode)
(global-set-key (kbd "C-c d h") 'dap-hydra)

(provide 'config-keybindings)
;;; config-keybindings.el ends here
