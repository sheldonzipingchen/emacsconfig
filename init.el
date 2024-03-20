;;; package --- Summary
;;; Commentary:
;;; Code:

(defvar cabins--os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins--os-mac (eq system-type 'darwin))

(defvar cabins--fonts-default '("Sometype Mono" "Cascadia Code PL" "Menlo" "Consolas"))
(defvar cabins--fonts-unicode '("Segoe UI Symbol" "Symbola" "Symbol"))
(defvar cabins--fonts-emoji '("Noto Color Emoji" "Apple Color Emoji"))
(defvar cabins--fonts-cjk '("KaiTi" "STKaiTi" "WenQuanYi Micro Hei"))

(require 'subr-x) ;; cl-loop来自这里

(defun cabins--set-font-common (character font-list &optional scale-factor)
  "Set fonts for multi CHARACTER from FONT-LIST and modify style with SCALE-FACTOR."

  (cl-loop for font in font-list
       when (find-font (font-spec :name font))
       return (if (not character)
              (set-face-attribute 'default nil :family font)
            (when scale-factor (setq face-font-rescale-alist `((,font . ,scale-factor))))
            (set-fontset-font t character (font-spec :family font) nil 'prepend))))

(defun cabins--font-setup (&optional default-fonts unicode-fonts emoji-fonts cjk-fonts)
  "Font setup, with optional DEFAULT-FONTS, UNICODE-FONTS, EMOJI-FONTS, CJK-FONTS."

  (interactive)
  (when (display-graphic-p)
    (cabins--set-font-common nil (if default-fonts default-fonts cabins--fonts-default))
    (cabins--set-font-common 'unicode (if unicode-fonts unicode-fonts cabins--fonts-unicode))
    (cabins--set-font-common 'emoji (if emoji-fonts emoji-fonts cabins--fonts-emoji))
    (dolist (charset '(kana han bopomofo cjk-misc))
      (cabins--set-font-common charset (if cjk-fonts cjk-fonts cabins--fonts-cjk) 1.2))))

(setq backup-directory-alist (quote (("." . "~/.backups"))))




(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t)

(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t  ; 注释贴右侧对齐
        company-tooltip-limit 20             ; 菜单里可选项数量
        company-show-quick-access t          ; 显示编号（然后可以用 M-数字 快速选定某一项）
        company-idle-delay .2                ; 延时多少秒后弹出
        company-minimum-prefix-length 1      ; 至少几个字符符后开始补全
        ))

(use-package format-all
  :ensure t
  :defer t
  ;; 开启保存时自动格式化
  :hook (prog-mode . format-all-mode)
  ;; 绑定一个手动格式化的快捷键
  :bind ("C-c f" . #'format-all-region-or-buffer))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

(add-hook 'prog-mode-hook 'column-number-mode)          ; 在 ModeLine 上显示列号
(add-hook 'prog-mode-hook 'display-line-numbers-mode)   ; 显示行号
(add-hook 'prog-mode-hook 'electric-pair-mode)          ; 括号配对

(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-language-source-alist
	'((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
	  (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	  (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
	  (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
	  (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
	  (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
	  (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
	  (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
	  (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
	  (make       . ("https://github.com/alemuller/tree-sitter-make"))
	  (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
	  (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
	  (org        . ("https://github.com/milisims/tree-sitter-org"))
	  (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	  (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
	  (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
	  (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
	  (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
	  (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
	  (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
	  (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
	  (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))
  (add-to-list 'major-mode-remap-alist '(sh-mode         . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode          . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode        . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode   . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode        . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode       . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode    . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(makefile-mode   . cmake-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode       . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(conf-toml-mode  . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))


(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure)
  :bind ("C-c e f" . eglot-format))

(add-to-list 'load-path "~/.emacs.d/beancount-mode")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
