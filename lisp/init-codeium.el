(add-to-list 'load-path "~/.emacs.d/codeium/")

(require 'codeium)

(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

(setq use-dialog-box nil)
(setq codeium-mode-line-enable
      (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
(add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

(setq codeium-api-enabled
      (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))


(defun my-codeium/document/text ()
  (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))

(defun my-codeium/document/cursor_offset ()
  (codeium-utf8-byte-length
   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
(setq codeium/document/text 'my-codeium/document/text)
(setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)

(provide 'init-codeium)
