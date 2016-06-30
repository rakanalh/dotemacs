(require 'org-projectile)

(setq org-directory "~/DropBox/org-mode")

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-hide-leading-stars t)

(org-projectile:per-repo)
(setq org-projectile:per-repo-filename "todo.org")
(setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(define-key global-map "\C-cc" 'org-capture)
