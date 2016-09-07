(use-package org
  :ensure t
  :config
  :config
  (setq org-directory "~/DropBox/org-mode"
        org-agenda-files (list "~/DropBox/org-mode/notes.org"
                             "~/DropBox/org-mode/todo.org"
                             "~/DropBox/org-mode/jordan-visit.org")
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (append org-agenda-files (org-projectile:todo-files))))

(use-package org-projectile
  :ensure t
  :config
  (setq org-projectile:per-repo-filename "todo.org")
  (org-projectile:per-repo))

(use-package org-bullets
  :ensure t
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))
