(setq confirm-kill-emacs #'yes-or-no-p)            ; 在关闭 Emacs 前询问是否确认关闭，防止误触
(electric-pair-mode t)                             ; 自动补全括号
(add-hook 'prog-mode-hook #'show-paren-mode)       ; 编程模式下，光标在括号上时高亮另一个括号
(column-number-mode t)                             ; 在 Mode Line 上显示列号
(global-auto-revert-mode t)                        ; 当另一程序修改了文件时，让 Emacs 及时刷新 Buffer
(delete-selection-mode t)                          ; 选中文本后输入文本会替换文本
(setq inhibit-startup-message t)                   ; 关闭启动 Emacs 时的欢迎界面
(setq make-backup-files nil)                       ; 关闭文件自动备份
(add-hook 'prog-mode-hook #'hs-minor-mode)         ; 编程模式下，可以折叠代码块
(global-display-line-numbers-mode 1)               ; 在 Window 显示行号
(tool-bar-mode 1)                                  ; 打开 Tool Bar
(when (display-graphic-p) (toggle-scroll-bar -1))  ; 图形界面时关闭滚动条
(savehist-mode 1)                                  ; 打开 Buffer 历史记录保存
(setq display-line-numbers-type 'relative)         ; 显示相对行号
(add-to-list 'default-frame-alist '(width . 90))   ; 设置启动图形界面时的初始 Frame 宽度（字符数）
(add-to-list 'default-frame-alist '(height . 55))  ; 设置启动图形界面时的初始 Frame 高度（字符数）

(add-to-list 'default-frame-alist
             '(font . "Ubuntu Mono-16"))

(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package gandalf-theme
  :ensure t
  :config
  (load-theme 'gandalf t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))


(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)  ; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers 1)  ;; 给选项编号（按快捷键 M-1、M-2 等等来进行选择）
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))) ; 根据选择的频率进行排序

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))


(use-package smart-mode-line
  :ensure t
  :init (sml/setup
	 sml/theme 'light
	 sml/theme 'respectful))


(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))


(use-package magit
  :ensure t)


(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages '(counsel ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
