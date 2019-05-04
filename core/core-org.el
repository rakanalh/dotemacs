(use-package org
  :custom
  (org-todo-keywords
   '(
     (sequence "IDEA(i)" "TODO(t)" "PENDING(p)" "STARTED(s)" "|" "POSTPONED(l)" "DONE(d)" "CANCELED(c)")
     ))

  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#41728e" :weight bold))
     ("STARTED" . (:foreground "#81a2be" :weight bold))
     ("PENDING" . (:foreground "#8abeb7" :slant italic))
     ("CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold :strike-through))
     ("POSTPONED" . (:foreground "#008080" :slant italic))))

  (org-directory "~/Documents/org-mode")

  (org-agenda-files (list "~/Documents/org-mode/inbox.org"
                          "~/Documents/org-mode/projects.org"
                          "~/Documents/org-mode/ideas.org"
                          "~/Documents/org-mode/learning.org"))

  (org-refile-targets '(("~/Documents/org-mode/projects.org" :maxlevel . 2)
                        ("~/Documents/org-mode/ideas.org" :maxlevel . 2)
                        ("~/Documents/org-mode/learning.org" :maxlevel . 2)))

  (org-default-notes-file (concat org-directory "/todo.org"))
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/Documents/org-mode/inbox.org" "Tasks")
      "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
     ("i" "Idea" entry (file+headline "~/Documents/org-mode/ideas.org" "Ideas")
      "* IDEA %?\nAdded: %U\n" :prepend t :kill-buffer t)
     ("l" "Learnings" entry (file+headline "~/Documents/org-mode/learning.org" "What i learned")
      "* Learned %?\nAdded: %U\n" :prepend t :kill-buffer t)
     ))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (shell . t)
                               (emacs-lisp . t)
                               (lisp . t)))
  :hook
  (org-finalize-agenda . (lambda ()
                                (setq org-agenda-tags-column (- 6 (window-width)))
                                (org-agenda-align-tags)))
  :bind
  ("\C-c o l" . org-store-link)
  ("\C-c o a" . org-agenda)
  ("\C-c o c" . org-capture))


(use-package org-alert
  :config
  (if (memq window-system '(mac ns))
      (setq alert-default-style 'osx-notifier)
    (setq alert-default-style 'libnotify))
  (org-alert-enable))

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "notes.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  :bind
  ("C-c o p" . org-projectile-capture-for-current-project))

(use-package org-bullets
  :custom
  (org-hide-leading-stars t)
  :hook
  (org-mode . (lambda () (org-bullets-mode t))))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode))

(provide 'core-org)
