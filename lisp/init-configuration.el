;; 初始化包管理器
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; 安装 use-package（如果未安装）
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 启用 use-package
(require 'use-package)
(setq use-package-always-ensure t)


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(fset 'yes-or-no-p 'y-or-n-p)

;; 自动断行
(global-visual-line-mode 1)
(setq-default truncate-lines nil)

;; 显示行号、高亮当前行
(global-display-line-numbers-mode t)
(global-hl-line-mode t)


;; 自动补全括号
(electric-pair-mode t)

;; 设置备份文件目录(避免污染当前目录)
(setq backup-directory-alist (quote (("." . "~/.backups"))))

;; 启用垃圾回收优化(减少卡顿)
(setq gc-cons-threshold (* 50 1024 1024)) ; 50MB

;; 设置字体和主题
(set-frame-font "FiraCode Nerd Font Propo 16" nil t)
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-soft t))


;; 快速按键提示(which-key)
(use-package which-key
  :ensure t
  :init (which-key-mode))


;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(provide 'init-configuration)
