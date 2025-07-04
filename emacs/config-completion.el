;;; config-completion.el --- Completion system configuration -*- lexical-binding: t; -*-

;; Completion framework
(use-package orderless
  :config
  (setq completion-styles '(orderless))
  (setq orderless-matching-styles 
        '(orderless-literal     ; Then literal substring matches
          orderless-regexp)))   ; Then regexp matches

(use-package vertico
  :init
  (vertico-mode)
  :config
  (defun my/vertico-sort-history-prefix-first (candidates)
    (let* ((input (minibuffer-contents-no-properties))
           ;; Extract just the part after the last / for file completion
           (input-basename (if (and minibuffer-completing-file-name
                                    (string-match "/\\([^/]*\\)$" input))
                               (match-string 1 input)
                             input))
           ;; Get the relevant history list
           (hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable)))
           ;; Categories for sorting
           (history-prefix-matches '())
           (history-other-matches '())
           (prefix-matches '())
           (other-matches '()))
      ;; Sort candidates into categories
      (dolist (cand candidates)
        (let* ((cand-str (substring-no-properties cand))
               (display-cand cand-str)
               (is-prefix (string-prefix-p input-basename display-cand 'ignore-case))
               (in-history (member cand-str hist)))
          (cond
           ;; History + prefix match (highest priority)
           ((and in-history is-prefix)
            (push cand history-prefix-matches))
           ;; History + other match
           (in-history
            (push cand history-other-matches))
           ;; Prefix match only
           (is-prefix
            (push cand prefix-matches))
           ;; Other matches
           (t
            (push cand other-matches)))))
      ;; Combine all groups in priority order
      (nconc (nreverse history-prefix-matches)
             (nreverse history-other-matches)
             (nreverse prefix-matches)
             (nreverse other-matches))))
  (setq vertico-sort-override-function #'my/vertico-sort-history-prefix-first))

(use-package vertico-posframe
	:config
	(vertico-posframe-mode 1))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-s"   . consult-line)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
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

;; Company completion
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
  (company-dabbrev-downcase nil)
  (company-tooltip-limit 20)
  (company-minimum-prefix-length 3)
  :config
  ;; History tracking
  (defvar my/company-history-table (make-hash-table :test 'equal)
    "Hash table to store completion history with timestamps.")

  ;; Record selected completions
  (defun my/company-record-completion (candidate)
    "Record the selected completion with current timestamp."
    (when candidate
      (puthash candidate (float-time) my/company-history-table)))

  (add-hook 'company-completion-finished-hook
            (lambda (candidate) (my/company-record-completion candidate)))
  ;; Custom transformation function with history and prefix priority
  (defun my/company-sort-history-prefix-first (candidates)
    (let* ((input company-prefix)
           (history-matches '())
           (prefix-matches '())
           (other-matches '()))
      (dolist (cand candidates)
        (cond
         ;; First priority: history matches that are also prefix matches
         ((and (gethash cand my/company-history-table)
               (string-prefix-p input cand))
          (push cand history-matches))
         ;; Second priority: prefix matches without history
         ((string-prefix-p input cand)
          (push cand prefix-matches))
         ;; Everything else
         (t
          (push cand other-matches))))
      ;; Sort history matches by recency (most recent first)
      (setq history-matches
            (sort history-matches
                  (lambda (a b)
                    (> (gethash a my/company-history-table 0)
                       (gethash b my/company-history-table 0)))))
      (nconc history-matches (nreverse prefix-matches) (nreverse other-matches))))
  ;; Add the sorting function to company transformers
  (add-to-list 'company-transformers #'my/company-sort-history-prefix-first))

(use-package cape)

;; Snippets
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (format "%s/%s" dotfiles-dir "yasnippets"))
  (yas-global-mode 1))
(use-package yasnippet-snippets)

;; Which-key
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

(provide 'config-completion)
;;; config-completion.el ends here
