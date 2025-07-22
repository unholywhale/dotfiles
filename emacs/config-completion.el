;;; config-completion.el --- Completion system configuration -*- lexical-binding: t; -*-

(setq tab-always-indent 'complete)

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

;; Corfu completion
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-count 10)
  (corfu-scroll-margin 5)
  :config
  ;; Enable corfu in the minibuffer
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  
  ;; History tracking for corfu
  (defvar my/corfu-history-table (make-hash-table :test 'equal)
    "Hash table to store completion history with timestamps.")
  
  (defun my/corfu-record-completion ()
    "Record the selected completion with current timestamp."
    (when (and corfu--candidates corfu--index)
      (let ((candidate (nth corfu--index corfu--candidates)))
        (when candidate
          (puthash candidate (float-time) my/corfu-history-table)))))
  
  (add-hook 'corfu-insert-hook #'my/corfu-record-completion))

;; Corfu extensions (built into corfu package)
(with-eval-after-load 'corfu
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (setq corfu-popupinfo-hide nil))

;; Terminal support for corfu
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :ensure t
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  
  ;; Ensure that pcomplete does not write to the buffer
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; Snippets
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (format "%s/%s" dotfiles-dir "yasnippets"))
  (yas-global-mode 1)
	:bind
	(:map yas-minor-mode-map
				("TAB" . nil)
				("<tab>" . nil)))
(use-package yasnippet-snippets)
(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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
