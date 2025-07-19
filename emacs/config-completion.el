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
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
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
  :init
  (global-company-mode)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0.2)
  (company-require-match nil)
  (company-echo-delay 0.1)
  (company-dabbrev-downcase nil)
  (company-tooltip-limit 10)
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
