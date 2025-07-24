;;; config-llm.el --- LLM configuration with ellama -*- lexical-binding: t; -*-

(defun my/get-openrouter-key ()
	"Get OpenRouter API key from 1password via auth-source."
	(let ((auth-info (auth-source-search :host "openrouter.ai" :user "api")))
		(when auth-info
			(plist-get (car auth-info) :secret))))

;; Ellama configuration
(use-package ellama
  :ensure t
  :init
  ;; Set up OpenRouter provider
  (require 'llm-openai)
  (setq ellama-provider
        (make-llm-openai-compatible
         :key (lambda () (my/get-openrouter-key))
         :url "https://openrouter.ai/api/v1"
         :chat-model "moonshotai/kimi-k2"))
	(setq llm-warn-on-nonfree nil)
  (setopt ellama-keymap-prefix "C-c a")
  :config
  ;; General ellama settings
  (setq ellama-language "English")
  (setq ellama-enable-keymap t))

;; Optional: Configure additional OpenRouter models
(defun my/ellama-switch-to-kimi-k2 ()
  "Switch ellama provider to Kimi K2 via OpenRouter."
  (interactive)
  (setq ellama-provider
        (make-llm-openai-compatible
         :key (lambda () (my/get-1password-secret "OpenRouter API" "credential"))
         :url "https://openrouter.ai/api/v1"
         :chat-model "moonshotai/kimi-k2"))
  (message "Switched to Kimi K2"))

(defun my/ellama-switch-to-qwen3-coder ()
  "Switch ellama provider to Qwen3 Coder via OpenRouter."
  (interactive)
  (setq ellama-provider
        (make-llm-openai-compatible
         :key (lambda () (my/get-1password-secret "OpenRouter API" "credential"))
         :url "https://openrouter.ai/api/v1"
         :chat-model "qwen/qwen3-coder"))
  (message "Switched to Qwen3 Coder"))

(defun my/ellama-switch-to-gpt4 ()
  "Switch ellama provider to GPT-4 via OpenRouter."
  (interactive)
  (setq ellama-provider
        (make-llm-openai-compatible
         :key (lambda () (my/get-1password-secret "OpenRouter API" "credential"))
         :url "https://openrouter.ai/api/v1"
         :chat-model "openai/gpt-4-turbo"))
  (message "Switched to GPT-4 Turbo"))

(defun my/ellama-switch-to-claude ()
  "Switch ellama provider to Claude 4 Sonnet via OpenRouter."
  (interactive)
  (setq ellama-provider
        (make-llm-openai-compatible
         :key (lambda () (my/get-1password-secret "OpenRouter API" "credential"))
         :url "https://openrouter.ai/api/v1"
         :chat-model "anthropic/claude-4-sonnet"))
  (message "Switched to Claude 3.5 Sonnet"))

;; Add which-key descriptions for ellama commands
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c a" "ellama/ai"))

(provide 'config-llm)
;;; config-llm.el ends here
