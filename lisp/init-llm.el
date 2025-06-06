;;; lisp/init-llm.el --- Init Ellama

(use-package ellama :ensure t
  ;; :bind ("C-c e" . ellama-transient-main-menu)
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
  	  (make-llm-ollama
  	   ;; this model should be pulled to use it
  	   ;; value should be the same as you print in terminal during pull
  	   :chat-model "phi4-mini"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 16384))))
  (setopt ellama-summarization-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
  	  (make-llm-ollama
  	   :chat-model "qwen2.5"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-extraction-provider (make-llm-ollama
  				      :chat-model "qwen2.5"
  				      :embedding-model "nomic-embed-text"
  				      :default-chat-non-standard-params '(("num_ctx" . 32768))))

  ;; send last message in chat buffer with C-c C-c
  ;; (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message)

  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom))

(provide 'init-llm)
