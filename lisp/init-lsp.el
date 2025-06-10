(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  
  :hook
  ((python-mode go-mode) . lsp-deferred)
  
  :init
  (setq lsp-keymap-prefix "C-c l"
		lsp-auto-configure t
		lsp-auto-guess-root t
		lsp-idle-delay 0.500
		lsp-session-file "~/.emacs/.cache/lsp-sessions")
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-log-io nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands (lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
		([remap xref-find-references] . lsp-ui-peek-find-references)
		([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
		("C-c u" . lsp-ui-imenu))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-enable-symbol-highlighting t
		lsp-ui-doc-enable t
		lsp-lens-enable t)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-delay 0.1)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.1)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-list-width 60)
  (setq lsp-ui-peek-peek-height 40))

;; 配置 Rust 的 LSP
(defun rust-lsp-config ()
  "配置 Rust 的 LSP 环境"
  (lsp-deferred) ; 异步启动 LSP
  (lsp-ui-mode) ; 启用 lsp-ui
  (company-mode) ; 启用自动补全
  (flycheck-mode) ; 启用语法检查
  
  ;; 特定于 rust-analyzer 的配置
  (setq-local lsp-rust-analyzer-cargo-watch-command "clippy") ; 使用 clippy
  (setq-local lsp-rust-analyzer-proc-macro-enable t)
  (setq-local lsp-rust-analyzer-diagnostics-enable t)
  (setq-local lsp-rust-analyzer-display-parameter-hints t)
  (setq-local lsp-rust-analyzer-max-inlay-hint-length 50)
  (setq-local lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"))
  
(add-hook 'rust-ts-mode-hook #'rust-lsp-config)


(provide 'init-lsp)
