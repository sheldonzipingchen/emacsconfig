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
  :config
  (setq lsp-auto-configure t))

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
		lsp-lens-enable t))

(provide 'init-lsp)
