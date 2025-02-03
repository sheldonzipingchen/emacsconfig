;;; package --- Summary
;;; Commentary:
;;; Code:

(setq user-full-name "Sheldon Chen"
      user-mail-address "sheldon.ziping.chen@gmail.com")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 定义快捷键快速打开 init.el
(defun open-init-file ()
  "打开 Emacs 配置文件"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c e") 'open-init-file)


;; 启用垃圾回收优化(减少卡顿)
(setq gc-cons-threshold (* 50 1024 1024)) ; 50MB


;; 启用原生编译(Emacs 28+)
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq package-native-compile t))


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
(eval-when-compile
  (require 'use-package))


;; 设置字体和主题
(set-frame-font "FiraCode Nerd Font Propo 14" nil t)
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-soft t))


;; 显示行号、高亮当前行
(global-display-line-numbers-mode t)
(global-hl-line-mode t)


;; 自动补全括号
(electric-pair-mode t)

;; 设置备份文件目录(避免污染当前目录)
(setq backup-directory-alist (quote (("." . "~/.backups"))))


;; 安装 beancount-mode
(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))


;; 快速按键提示(which-key)
(use-package which-key
  :ensure t
  :init (which-key-mode))


;; 模糊搜索(ivy/counsel/swiper)
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x) ; 替换默认 M-x
	 ("C-s" . swiper)))


;; 自动补全(company)
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1))


;; 图标支持(nerd-icons)
(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))


;; 现代模式行(doom-modeline)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode)
  :config
  (setq doom-modeline-height 25))


;; 语法检查(flycheck)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; 版本控制(magit)
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; 格式化插件(format-all)
(use-package format-all
  :ensure t
  :defer t
  ;; 开启保存时自动格式化
  :hook (prog-mode . format-all-mode)
  ;; 绑定手动格式化的快捷键
  :bind ("C-c f" . #'format-all-region-or-buffer))


;; pyvenv
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; 设置Python解释器
  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python3")))))


;; Org Mode 基础配置
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
		      (org-indent-mode) ; 启用缩进对齐
		      (visual-line-mode))) ; 自动折行
  :config
  (setq org-log-done 'time) ; 任务完成时记录时间
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(pyvenv format-all magit flycheck gruvbox)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
