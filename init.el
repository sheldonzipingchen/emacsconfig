;;; package --- Summary
;;; Commentary:
;;; Code:

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

;; 设置字体和主题
(set-frame-font "FiraCode Nerd Font Propo 16" nil t)
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-soft t))


;; valign
(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode))


;; 快速按键提示(which-key)
(use-package which-key
  :ensure t
  :init (which-key-mode))


;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))


;; magit
(use-package magit
  :ensure t)


;; Org
(use-package org
  :ensure t
  :defer t
  :init
  ;; 基础目录与文件设置
  (setq org-directory "~/projects/notes/")
  (setq org-agenda-files (list (concat org-directory "tasks.org")))
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-diary-note-file (concat org-directory "diary.org"))

  ;; 预加载常用贡献包
  (require 'org-contrib nil t)

  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c t" . org-todo-list))
  
  :config
  ;; ------------------------
  ;; 核心任务管理
  ;; ------------------------
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-log-done 'time)  ; 记录任务完成时间

  ;; ------------------------
  ;; 快速捕获模板
  ;; ------------------------
  (setq org-capture-templates
	'(("t" "Task" entry (file+headline org-default-notes-file "Tasks") "* TODO %?\n %i\n %a")
	  ("n" "Note" entry (file+headline org-default-notes-file "Notes") "* %?\n %i\n %a")
	  ("d" "Diary" entry (file+datetree "~/projects/notes/diary.org")
	   "* %?\n%U\n" :clock-in t :clock-resume t)
	  ))

  ;; -----------------------
  ;; 界面与编辑优化
  ;; -----------------------
  (setq org-src-fontify-natively t)    ; 代码块语法高亮
  (setq org-src-tab-acts-natively t)  ; 代码块原生缩进
  (setq org-startup-indented t)       ; 标题自动缩进
  (setq org-hide-emphasis-markers t)  ; 隐藏 *粗体*/_斜体_ 符号

  ;; 设置标题字体大小
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (set-face-attribute 'org-level-2 nil :height 1.1)

  ;; --------------------------
  ;; 高级功能扩展
  ;; --------------------------
  ;; 启用 org-roam（知识图谱笔记）
  (use-package org-roam
    :ensure t
    :init
    (setq org-roam-directory (concat org-directory "roam/"))
    :config
    (org-roam-db-autosync-mode))

  ;; 时间追踪与报表
  (setq org-clock-report-include-clocking-task t)
  (setq org-log-repeat 'time))


(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t
		company-tooltip-limit 20
		company-show-numbers t
		company-idle-delay .2
		company-minimum-prefix-length 1))

(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  :hook
  (before-save . gofmt-before-save)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports))

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

(add-to-list 'load-path "~/.emacs.d/beancount-mode/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ui lsp-mode go-mode valign cnfonts org-table-auto-align-mode org-table-mode org-table org-roam magit exec-path-from-shell which-key gruvbox-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
