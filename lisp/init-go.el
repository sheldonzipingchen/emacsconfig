(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'go-mode-hook (lambda () (setq indent-tabs-mode 1)))
  :hook
  (before-save . gofmt-before-save)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports))

(provide 'init-go)
