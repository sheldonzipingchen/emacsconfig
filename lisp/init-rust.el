(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t
  :hook (rust-ts-mode . cargo-minor-mode)
  :config
  (setq cargo-process--command-build "check")

  ;; 自定义 Cargo 命令
  (defun cargo-run-release ()
    "运行 cargo build --release"
    (interactive)
    (cargo-process--start "build" "--release")
    (cargo-process-run))

  (defun cargo-test-all ()
    "运行所有测试"
    (interactive)
    (cargo-process--start "test" "--all")))


;; Rust 代码格式配置
(use-package rustic
  :ensure t
  :config
(setq rustic-rustfmt-config-alist '((tab_spaces . 4) (edition . "2021"))))

;; ========================
;; 实用键绑定
;; ========================
(defun rust-keybindings ()
  "Rust 专用键绑定"
  ;; 代码导航
  (define-key rust-ts-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key rust-ts-mode-map (kbd "M-,") 'lsp-find-references)
  (define-key rust-ts-mode-map (kbd "M-?") 'lsp-find-implementation)
  
  ;; 代码操作
  (define-key rust-ts-mode-map (kbd "C-c r") 'rust-analyzer-runnables)
  (define-key rust-ts-mode-map (kbd "C-c t") 'rust-test)
  
  ;; 文档
  (define-key rust-ts-mode-map (kbd "C-c d") 'lsp-describe-thing-at-point)
  
  ;; 格式化
  (define-key rust-ts-mode-map (kbd "C-M-\\") 'rust-format-buffer)
  
  ;; 调试
  (define-key rust-ts-mode-map (kbd "<f5>") 'dap-debug)
  (define-key rust-ts-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
  
  ;; Cargo
  (define-key rust-ts-mode-map (kbd "C-c C-c c") 'cargo-process-check)
  (define-key rust-ts-mode-map (kbd "C-c C-c b") 'cargo-process-build)
  (define-key rust-ts-mode-map (kbd "C-c C-c t") 'cargo-process-test)
  (define-key rust-ts-mode-map (kbd "C-c C-c r") 'cargo-process-run))

(add-hook 'rust-ts-mode-hook 'rust-keybindings)


(provide 'init-rust)
