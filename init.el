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


;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


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


;; 通用 LSP 配置
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  ;; 在 prog-mode 中延迟启动 LSP(提升性能)
  (setq lsp-auto-guess-root t) ; 自动检测项目根目录
  (setq lsp-enable-snippet t)  ; 启用代码片段支持
  (setq lsp-enable-indentation nil) ; 禁用 LSP 缩进
  :hook
  (prog-mode . lsp-deferred)) ; 延迟加载 LSP

;; 自动补全(company)
(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1
	lsp-completion-provider :capf))


;; LSP 界面增强
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t) ; 启用悬浮文档
  (setq lsp-ui-sideline-enable t) ; 启用右侧诊断信息
  (setq lsp-ui-peek-always-show t)) ; 启用代码跳转预览


;; 语法检查(flycheck)
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))


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


;; 启用代码括号匹配高亮
(use-package paren
  :config
  (show-paren-mode 1))


;; Org Mode 基础配置
(use-package org
  :ensure nil
  :hook (org-mode . (lambda ()
		      (org-indent-mode) ; 启用缩进对齐
		      (visual-line-mode))) ; 自动折行
  :config
  (setq org-directory "~/projects/notes/") ; 笔记目录
  (setq org-agenda-files '("~/projects/notes/Tasks.org")) ; 指定议程文件
  (setq org-log-done 'time) ; 任务完成时记录时间

  ;; 设置任务状态流程
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(d)" "BLOCKED(b)" "|" "DONE(D)" "CANCELLED(c)")))

  ;; 高亮代码块语法
  (setq org-src-fontify-natively t))

;; 快速打开 org 文件的快捷键
(global-set-key (kbd "C-c o") (lambda () (interactive) (find-file (concat org-directory "/Tasks.org"))))

;; 设置任务计划时间和截止时间
(setq org-deadline-warning-days 3) ; 截止日期前3天提醒

;; 任务时间追踪
;; 快速插入时间戳的快捷键
(define-key org-mode-map (kbd "C-c .") 'org-time-stamp) ; 插入活动时间戳（计划时间）
(define-key org-mode-map (kbd "C-c !") 'org-time-stamp-inactive) ; 插入非活动时间戳（日志记录）

;; 议程视图(agenda)
;; 绑定议程视图快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

;; 议程视图配置
(setq org-agenda-span 7) ; 默认显示7天任务
(setq org-agenda-start-with-log-mode t) ; 显示任务日志
(setq org-agenda-skip-deadline-if-done t) ; 跳过已完成任务的截止日期

;; 代码块执行(Babel)
;; 启用代码执行
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) ; 执行 Python 代码
   (shell . t) ; 执行 Shell 命令
   (emacs-lisp . t) ; 执行 Elisp 代码
   (plantuml . t))) ; 绘图（需安装 plantuml.jar）

;; 设置 PlantUML 路径
(setq org-plantuml-jar-path "~/.emacs.d/lib/plantuml.jar")

;; 快速公式计算(按 `C-c +` 或 `C-c -` 自动计算)
(setq org-table-use-standard-references t) ; 使用 Excel 风格的公式(如B3)

;; 设置标题字体大小和颜色
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4 :foreground "#E06C75"))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3 :foreground "#98C379")))))

;; 优化列表缩进
(setq org-list-indent-offset 2) ; 子列表缩进2字符

;; 在 Org Mode 中使用图标
(setq org-ellipsis " ") ; 替换默认的折叠符号 "..."


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


;; Go 专用配置
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook
  (before-save . gofmt-before-save)
  (go-mode . (lambda ()
	       (setq tab-width 4) ; 设置缩进为 4 空格
	       (setq indent-tabs-mode t))) ; Go 使用 Tab 缩进
  :config
  (setq gofmt-command "goimports")
  ;; 绑定常用快捷键
  (define-key go-mode-map (kbd "C-c C-g") 'go-goto-imports)
  (define-key go-mode-map (kbd "C-c C-f") 'gofmt) ; 格式化代码
  
  ;; 设置 Go 格式化工具
  (add-hook 'before-save-hook #'lsp-format-buffer t t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-mode lsp-ui lsp-mode exec-path-from-shell pyvenv format-all magit flycheck gruvbox)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
