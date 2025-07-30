;;; config-llm.el --- LLM configuration with ellama -*- lexical-binding: t; -*-

(defun my/get-openrouter-key ()
	"Get OpenRouter API key from 1password via auth-source."
	(let ((auth-info (auth-source-search :host "openrouter.ai" :user "api")))
		(when auth-info
			(plist-get (car auth-info) :secret))))

;; Define models as data
(defvar my/openrouter-models
  '(("moonshotai/kimi-k2" . "Kimi K2")
    ("qwen/qwen3-coder" . "Qwen3 Coder")
    ("openai/gpt-4-turbo" . "GPT-4 Turbo")
    ("anthropic/claude-4-sonnet" . "Claude Sonnet")))

(defun my/ellama-switch-to-model (model-name display-name)
  "Switch ellama provider to MODEL-NAME via OpenRouter."
  (message "Debug: Switching to model %s (%s)" model-name display-name)
  (let ((api-key (my/get-openrouter-key)))
    (message "Debug: API key retrieved: %s" (if api-key "YES" "NO"))
    (if api-key
        (condition-case err
            (progn
              (message "Debug: Creating provider...")
              (setq ellama-provider
                    (make-llm-openai-compatible
                     :key (lambda () api-key)
                     :url "https://openrouter.ai/api/v1"
                     :chat-model model-name))
              (message "Switched to %s" display-name))
          (error (message "Debug: Error creating provider: %s" err)))
      (error "Failed to get API key from 1Password"))))

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
  (setq ellama-enable-keymap t)
	(with-eval-after-load 'which-key
		(which-key-add-key-based-replacements
			"C-c a" "ellama/ai"
			"C-c a c" "code"
			"C-c a s" "summarize/session"
			"C-c a i" "improve"
			"C-c a m" "make"
			"C-c a a" "ask"
			"C-c a t" "text/translate"
			"C-c a d" "define"
			"C-c a x" "context"
			"C-c a p" "provider")))



;; Add providers - delay execution to ensure functions are loaded
(eval-after-load 'ellama
  '(progn
     (setq ellama-providers
           (append ellama-providers
                   (list (cons "Kimi K2" '(make-llm-openai-compatible
                                           :key (lambda () (my/get-openrouter-key))
                                           :url "https://openrouter.ai/api/v1"
                                           :chat-model "moonshotai/kimi-k2"))
                         (cons "GPT-4 Turbo" '(make-llm-openai-compatible
                                               :key (lambda () (my/get-openrouter-key))
                                               :url "https://openrouter.ai/api/v1"
                                               :chat-model "openai/gpt-4-turbo"))
                         (cons "Qwen3 Coder" '(make-llm-openai-compatible
                                               :key (lambda () (my/get-openrouter-key))
                                               :url "https://openrouter.ai/api/v1"
                                               :chat-model "qwen/qwen3-coder"))
                         (cons "Claude Sonnet" '(make-llm-openai-compatible
                                                 :key (lambda () (my/get-openrouter-key))
                                                 :url "https://openrouter.ai/api/v1"
                                                 :chat-model "anthropic/claude-4-sonnet")))))))

;; ChatGPT Shell configuration
(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package chatgpt-shell
  :straight (:type git
									 :host github
									 :repo "xenodium/chatgpt-shell")
									 ;; :local-repo "~/personal/chatgpt-shell/"
									 ;; :files ("*.el" "*.png")
									 )
	;;(:host github :repo "xenodium/chatgpt-shell")
  :config
  ;; Load OpenRouter provider
  (require 'chatgpt-shell-openrouter)
  ;; Set OpenRouter key
  (setq chatgpt-shell-openrouter-key (lambda () (my/get-openrouter-key)))
	(setq chatgpt-shell-model-version "moonshotai/kimi-k2")

  ;; Add custom OpenRouter models
  (setq chatgpt-shell-models
        (append chatgpt-shell-models
                (list
                 (chatgpt-shell-openrouter-make-model
                  :label "Kimi K2"
                  :version "moonshotai/kimi-k2"
                  :short-version "kimi-k2"
                  :token-width 4
                  :context-window 128000)
                 (chatgpt-shell-openrouter-make-model
                  :label "Qwen3 Coder"
                  :version "qwen/qwen3-coder"
                  :short-version "qwen3-coder"
                  :token-width 4
                  :context-window 256000))))

  ;; Set default model (with openrouter/ prefix)
  ;;(setq chatgpt-shell-model-version "moonshotai/kimi-k2")
  ;; Keybindings
  :bind
  (("C-c g c" . chatgpt-shell)
   ("C-c g r" . chatgpt-shell-prompt-region)
   ("C-c g b" . chatgpt-shell-prompt-buffer)))


;; Add which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c g" "chatgpt-shell"))


(provide 'config-llm)
;;; config-llm.el ends here
