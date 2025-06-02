;;; package --- Summary
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-configuration)
(require 'init-editor)
(require 'init-projectile)
(require 'init-org)
(require 'init-lsp)
(require 'init-go)
(require 'init-yaml)
(require 'init-beancount)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(codeium projectile yaml-mode yasnippet-snippets lsp-ui lsp-mode go-mode valign cnfonts org-table-auto-align-mode org-table-mode org-table org-roam magit exec-path-from-shell which-key gruvbox-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
