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
		   (bash-mode . bash-ts-mode)
		   (rust-mode . rust-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; 自定义 Rust Tree-sitter 配置
(defun rust-treesit-config ()
  "配置 Rust Tree-sitter 模式"
  ;; 语法高亮级别 (0-4)
  (setq-local treesit-font-lock-level 4)
  
  ;; 基于 Tree-sitter 的缩进规则
  (setq-local treesit-simple-indent-rules
              '((rust
                 ((parent-is "block") parent-bol 2)
                 ((parent-is "function_item") parent-bol 2)
                 ((parent-is "impl_item") parent-bol 2)
                 ((parent-is "match_arm") parent-bol 2)
                 ((parent-is "use_declaration") parent-bol 2)
                 ((parent-is "struct_item") parent-bol 2)
                 ((node-is "else") parent-bol 0)
                 ((node-is ")") parent-bol 0)
                 ((node-is "}") parent-bol 0)
                 ((node-is "]") parent-bol 0))))
  
  ;; 启用基于 Tree-sitter 的代码导航
  (treesit-inspect-mode 1)
  
  ;; 使用制表符缩进 (Rust 标准)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  
  ;; 函数折叠支持
  (when (fboundp 'treesit-fold-mode)
    (treesit-fold-mode 1)))

(add-hook 'rust-ts-mode-hook #'rust-treesit-config)



(provide 'init-treesitter)
