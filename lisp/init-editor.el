;; magit
(use-package magit
  :ensure t)


;; company
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq
   company-tooltip-align-annotations t
   company-tooltip-limit 20
   company-show-numbers t
   company-idle-delay .2
   company-require-match nil
   company-minimum-prefix-length 0

   company-frontends '(company-preview-frontend)))


;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))


;; yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(setq user-snippet-dir "~/.emacs.d/snippets")

(unless (file-directory-p user-snippet-dir)
  (make-directory user-snippet-dir t))

(add-to-list 'yas-snippet-dirs user-snippet-dir)
(yas-reload-all)

(provide 'init-editor)
