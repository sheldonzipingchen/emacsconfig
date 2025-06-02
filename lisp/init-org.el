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

  (setq org-agenda-files '("~/projects/notes/notes.org"
						   "~/projects/notes/diary.org"))

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

;; valign
(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(provide 'init-org)
