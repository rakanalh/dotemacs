(require 'magit-git)
(require 'magit-process)
;(require 'persp-mode)

;; Delete words
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; COPY / PASTE on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun bookmark-jump-or-find-file (bookmark)
  "Jump to BOOKMARK, but if it's a directory, start a 'find-file' from there."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark")))
  (if (file-directory-p (bookmark-get-filename bookmark))
      (let ((default-directory (bookmark-get-filename bookmark)))
        (counsel-find-file))
    (bookmark-jump bookmark)))

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun disable-neotree-hook (_unused)
  (linum-mode -1))

(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; (defun persp/get-root (branch-name)
;;   (let ((current-project (projectile-project-name)))
;;     (message current-project)
;;     (message branch-name)
;;     (if (and (not current-project) (not branch-name))
;;       (error "Could not find persp root"))

;;     (if (and current-project branch-name)
;; 	(concat current-project "-" branch-name)
;;       (if current-project
;; 	  current-project
;; 	branch-name))))


;; (defun persp/close-perspective (&optional project-root closed-branch)
;;   (interactive)
;;   (let* ((current-branch (if closed-branch
;; 			    closed-branch
;; 			  (magit-get-current-branch)))
;; 	 (persp-project-root (if project-root
;; 				 project-root
;; 			       (persp/get-root current-branch))))
;;     (if persp-project-root
;; 	(progn
;; 	  (message (concat "Saving " persp-project-root ".persp"))
;; 	  (persp-save-state-to-file (concat persp-project-root ".persp"))
;; 	  (close-all-buffers)))))

;; (defun persp/switch-to-current-branch-persp ()
;;   (interactive)
;;   (let ((closed-branch (magit-get-previous-branch))
;; 	(persp-project-root (persp/get-root (magit-get-current-branch)))
;;     (persp/close-perspective persp-project-root closed-branch))
;;     (message "Closed perspective")
;;     (message (concat "Loading " persp-project-root ".persp"))
;;     (persp-load-state-from-file (concat persp-project-root ".persp"))))

(provide 'core-functions)
