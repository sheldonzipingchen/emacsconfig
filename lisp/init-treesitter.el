;; Tree-sitter 核心配置
(use-package treesit
  :ensure nil
  :config
  (setq treesit-font-lock-level 4)
  (setq treesit-inspect-indicator t))

;; 自动管理语法库
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (global-treesit-auto-mode t))

;; 替换默认模式
(dolist (mapping '((python-mode . python-ts-mode)
		   (js-mode . js-ts-mode)
		   (c-mode . c-ts-mode)
		   (c++-mode . c++-ts-mode)
		   (go-mde . go-ts-mode)
		   (bash-mode . bash-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(provide 'init-treesitter)
