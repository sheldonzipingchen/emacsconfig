(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-project-root-files '(".projectile" ".git" ".hg" ".svn" "Makefile"))
  (setq projectile-globally-ignored-files '(".DS_Store" "Icon"))
  (setq projectile-globally-ignored-directories '(".idea" ".vscode" ".cache" ".git"))
  (setq projectile-enable-caching t)
  (setq projectile-grep-tool 'ripgrep))

(provide 'init-projectile)
